package islay.template

import java.io.FileNotFoundException
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import scala.xml.{Comment, Elem, NodeSeq}

import akka.actor._
import akka.spray.UnregisteredActorRef
import islay.transform.CallingThreadExecutor
import spray.http.{EmptyEntity, HttpBody, HttpHeader, HttpMethods, HttpProtocols, HttpRequest, HttpResponse}
import spray.routing.{RequestContext, Route, RouteConcatenation}


case class TemplateProcessor(

  root: Path = Resources.pathTo("webapp"),
  formatter: Formatter = new Html5Formatter,
  parsers: Map[String, Parser] = Map("html" -> new Html5Parser),
  executor: ExecutionContext = ExecutionContext.global

) extends RouteConcatenation {
  import TemplateDirectives._


  implicit val self = this

  private val cache = "TODO"


  def route: Route = { context =>
    implicit val e = executor
    template(context.request.path, context)
  }


  /**
   * Returns a template parsed from the resource found at the given path (of the `HttpRequest`)
   * relative to this processor's root. If the path doesn't exactly match a resource or there isn't
   * a parser for the resource's file extension, then this method will look for a resource by
   * appending the file extension of each available parser until a match is made. If the path
   * matches a directory then a resource named "index" in that directory will be searched for
   * instead.
   *
   * @throws FileNotFoundException inside a [[scala.util.Failure]] if the resource cannot be found
   */
  def lookup(request: HttpRequest): Future[NodeSeq] = {
    implicit val e = executor

    Future {
      resolvePath(request.path)

    } flatMap { case (path, parser) =>
      val startBytes = Resources.readAllBytes(path)
      val allBytes = findSurroundEndPath(request) match {
        case None =>
          startBytes
        case Some(endPath) =>
          val endBytes = Resources.readAllBytes(resolvePath(endPath)._1)
          for (start <- startBytes; end <- endBytes)
          yield start ++ parser.contentBinding ++ end
      }
      allBytes.map(parser.parse)
    }
  }

  private def resolvePath(path: String): (Path, Parser) = {
    val exactPath = Resources.resolve(root, path.replaceFirst("\\?.*", ""))

    parsers find { case (extension, _) =>
      exactPath.getFileName.toString.endsWith("."+ extension) && Files.exists(exactPath)

    } map { case (_, parser) =>
      (exactPath, parser)

    } getOrElse {
      val fullPath = if (Files.isDirectory(exactPath)) exactPath.resolve("index") else exactPath

      @tailrec
      def loop(parsers: List[(String, Parser)]): (Path, Parser) =
        parsers match {
          case Nil => throw new FileNotFoundException(path)
          case (extension, parser) :: tail =>
            val extendedPath = fullPath.getParent.resolve(fullPath.getFileName +"."+ extension)
            if (Files.exists(extendedPath))
              (extendedPath, parser)
            else
              loop(tail)
        }
      loop(parsers.toList)
    }
  }

  private def findSurroundEndPath(request: HttpRequest): Option[String] =
    request.headers collectFirst {
      case SurroundEnd(path) => path
    }

  /**
   * Recursively applies any SSI templates using this processor's `route` to resolve included
   * content.
   */
  def expand(nodes: NodeSeq, context: RequestContext): Future[NodeSeq] = {
    /* remove temporary routing/lookup headers so that they don't apply recursively */
    val filteredHeaders = context.request.headers filterNot { h =>
      h.isInstanceOf[SurroundEnd] || h == IncludeMarker
    }
    val filteredRequest = context.request.copy(headers = filteredHeaders)
    val filteredContext = context.copy(request = filteredRequest)

    maybeExpand(nodes, filteredContext) getOrElse Future.successful(nodes)
  }

  private type NodeReplacement = (Future[NodeSeq], Int, Int)

  private def maybeExpand(nodes: NodeSeq, context: RequestContext): Option[Future[NodeSeq]] = {

    @tailrec
    def getReplacements(i: Int, replacements: List[NodeReplacement]): List[NodeReplacement] = {
      if (i >= nodes.length)
        replacements
      else {
        val maybeReplacement =
          nodes(i) match {
            case elem: Elem =>
              import CallingThreadExecutor.Implicit
              maybeExpand(elem.child, context).map(f => (f.map(ns => elem.copy(child = ns)), i))
            case comment: Comment =>
              maybeInclude(comment, i)
            case _ =>
              None
          }
        maybeReplacement match {
          case Some((r, end)) =>
            getReplacements(end+1, (r, i, end) :: replacements)
          case None =>
            getReplacements(i+1, replacements)
        }
      }
    }

    def maybeInclude(comment: Comment, i: Int): Option[(Future[NodeSeq], Int)] = {
      ssiUri(comment) map { uri =>
        findEndSsi(i+1) match {

          case Some((endUri, end)) =>
            val includeRequest = context.request.withHeaders(SurroundEnd(endUri) :: context.request.headers)
            val includeContext = context.copy(request = includeRequest)
            import CallingThreadExecutor.Implicit
            val included = for {
              includeNodes <- resolveInclude(uri, includeContext)
              surrounded = nodes.slice(i+1, end)
              content <- maybeExpand(surrounded, context) getOrElse Future.successful(surrounded)
            } yield bindContent(includeNodes, content) getOrElse sys.error("Could not find binding in surrounded content")
            (included, end)

          case _ =>
            (resolveInclude(uri, context), i)
        }
      }
    }

    @tailrec
    def findEndSsi(i: Int): Option[(String, Int)] = {
      if (i < nodes.length) {
        nodes(i) match {
          case comment: Comment =>
            for (path <- ssiUri(comment) if path.endsWith("?!"))
            yield (path, i)
          case _ =>
            findEndSsi(i+1)
        }
      }
      else
        None
    }

    replaceNodes(nodes, getReplacements(0, Nil))
  }

  private def bindContent(nodes: NodeSeq, content: NodeSeq): Option[NodeSeq] = {

    @tailrec
    def findReplacement(i: Int): Option[(NodeSeq, Int)] = {
      if (i >= nodes.length)
        None
      else {
        val maybeReplacement =
          nodes(i) match {
            case elem: Elem if elem.prefix == "islay" && elem.label == "binding" =>
              Some(content -> i)
            case elem: Elem =>
              bindContent(elem.child, content) map { ns =>
                elem.copy(child = ns) -> i
              }
            case _ =>
              None
          }
        maybeReplacement match {
          case None => findReplacement(i+1)
          case some => some
        }
      }
    }

    findReplacement(0) map { case (replacement, index) =>
      nodes.take(index) ++ replacement ++ nodes.drop(index+1)
    }
  }

  private def replaceNodes(nodes: NodeSeq, replacements: List[NodeReplacement]): Option[Future[NodeSeq]] = {
    import CallingThreadExecutor.Implicit

    if (replacements.isEmpty)
      None
    else {
      var remaining = replacements.reverse
      var newNodes = List[Future[NodeSeq]]()
      var i = 0
      while (i < nodes.length) {
        newNodes ::= (remaining match {
          case (replacement, start, end) :: tail if start == i =>
            remaining = tail
            i = end
            replacement
          case _ =>
            Future.successful(nodes(i))
        })
        i = i+1
      }
      Some(Future.sequence(newNodes).map(_.reverse.flatten))
    }
  }

  private def resolveInclude(ssiUri: String, context: RequestContext): Future[NodeSeq] = {
    val request = context.request
    val templateRequest = request.copy(
      method = HttpMethods.GET,
      uri = ssiUri,
      headers = IncludeMarker :: request.headers,
      entity = EmptyEntity,
      protocol = HttpProtocols.`HTTP/1.1`
    ).parseUri

    val promise = Promise[NodeSeq]
    val responder = new TemplateResponder(promise, context.responder)

    val templateContext = RequestContext(templateRequest, responder, templateRequest.path)
    route(templateContext)

    promise.future
  }

  private val ssiRegex = """\s*#include (?:virtual|file)="(.*?)"\s*""".r

  private def ssiUri(comment: Comment): Option[String] =
    ssiRegex findFirstMatchIn comment.commentText map (_ group 1)


  def format(nodes: NodeSeq): HttpResponse =
    HttpResponse(entity = HttpBody(formatter.contentType, formatter.format(nodes)))
}


private[islay] class TemplateResponder(promise: Promise[NodeSeq], delegate: ActorRef)
extends UnregisteredActorRef(delegate) {

  override def handle(message: Any)(implicit sender: ActorRef) {
    message match {
      case nodes: NodeSeq =>
        promise.complete(Success(nodes))
      case Status.Failure(ex) =>
        promise.complete(Failure(ex))
    }
  }
}

/**
 * Used by `completeTemplate()` to decide whether a response should be forwarded as a NodeSeq for
 * inclusion in the surrounding document or as the final `HttpResponse`.
 */
private[islay] case object IncludeMarker extends HttpHeader {
  def name = "X-Islay-Include"
  def lowercaseName = "x-islay-include"
  def value = ""
}

/**
 * Passed from `expand()` to the route for the include when a surrounding SSI pair is encountered
 * so that `lookup()` can parse both the start and end fragments as a single template.
 */
private[islay] case class SurroundEnd(uri: String) extends HttpHeader {
  def name = "X-Islay-Surround-End"
  def lowercaseName = "x-islay-surround-end"
  def value = ""
}