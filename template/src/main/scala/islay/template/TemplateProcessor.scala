package islay.template

import islay.transform.CallingThreadExecutor

import java.io.FileNotFoundException
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}
import scala.xml.{Comment, Elem, NodeSeq}

import akka.actor.{ActorRef, Status}
import akka.spray.UnregisteredActorRef
import spray.http.{EmptyEntity, HttpMethods, HttpProtocols, HttpRequest}
import spray.routing.{RequestContext, Route}


case class TemplateProcessor(
  route: Route = ???,
  root: Path = Resources.pathTo("webapp"),
  formatter: Formatter = ???,
  parsers: Map[String, Parser] = Map("html" -> ???)
) {


  private val cache = "TODO"


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
    import ExecutionContext.Implicits.global

    val exactPath = Resources.resolve(root, request.path)

    Future {
      parsers find { case (extension, _) =>
        exactPath.getFileName.toString.endsWith("."+ extension) && Files.exists(exactPath)

      } map { case (_, parser) =>
        (exactPath, parser)

      } getOrElse {
        val fullPath = if (Files.isDirectory(exactPath)) exactPath.resolve("index") else exactPath

        @tailrec
        def loop(parsers: List[(String, Parser)]): (Path, Parser) =
          parsers match {
            case Nil => throw new FileNotFoundException(request.path)
            case (extension, parser) :: tail =>
              val extendedPath = fullPath.getParent.resolve(fullPath.getFileName +"."+ extension)
              if (Files.exists(extendedPath))
                (extendedPath, parser)
              else
                loop(tail)
          }
        loop(parsers.toList)
      }

    } flatMap { case (path, parser) =>
      Resources.readAllBytes(path) map parser.parse
    }
  }

  type NodeReplacement = (Future[NodeSeq], Int)

  /**
   * Recursively applies any SSI templates using the `route` this processor was constructed with to
   * resolve included content.
   */
  def expand(nodes: NodeSeq, context: RequestContext): Option[Future[NodeSeq]] = {
    @tailrec
    def getReplacements(i: Int, replacements: List[NodeReplacement]): List[NodeReplacement] = {
      if (i >= nodes.length)
        replacements
      else {
        val maybeReplacement =
          nodes(i) match {
            case elem: Elem =>
              import CallingThreadExecutor.Implicit
              expand(elem.child, context).map(_.map(ns => elem.copy(child = ns)))
            case comment: Comment =>
              ssiUri(comment).map(uri => resolve(uri, context))
            case _ =>
              None
          }
        maybeReplacement match {
          case Some(r) =>
            getReplacements(i+1, (r, i) :: replacements)
          case None =>
            getReplacements(i+1, replacements)
        }
      }
    }

    replaceNodes(nodes, getReplacements(0, Nil))
  }

  private def replaceNodes(nodes: NodeSeq, replacements: List[NodeReplacement]): Option[Future[NodeSeq]] = {
    import CallingThreadExecutor.Implicit

    if (replacements.isEmpty)
      None
    else {
      var remaining = replacements.reverse
      val newNodes = Future.traverse(nodes.zipWithIndex) { case (node, i) =>
        remaining match {
          case (replacement, index) :: tail if index == i =>
            remaining = tail
            replacement
          case _ =>
            Future.successful(node)
        }
      }
      Some(newNodes.map(_.flatten))
    }
  }

  private def resolve(ssiUri: String, context: RequestContext): Future[NodeSeq] = {
    val request = context.request
    val templateRequest = request.copy(
      method = HttpMethods.GET,
      uri = ssiUri,
      headers = request.headers,
      entity = EmptyEntity,
      protocol = HttpProtocols.`HTTP/1.1`
    ).parseUri

    val promise = Promise[NodeSeq]
    val responder = new UnregisteredActorRef(context.responder) {
      override def handle(message: Any)(implicit sender: ActorRef) {
        message match {
          case nodes: NodeSeq =>
            promise.complete(Success(nodes))
          case Status.Failure(ex) =>
            promise.complete(Failure(ex))
        }
      }
    }

    val templateContext = RequestContext(templateRequest, responder, templateRequest.path)
    route(templateContext)

    promise.future
  }

  private val ssiRegex = """#include (?:virtual|file)="(.*?)"\s*""".r

  private def ssiUri(comment: Comment): Option[String] =
    ssiRegex findFirstMatchIn comment.commentText map (_ group 1)
}