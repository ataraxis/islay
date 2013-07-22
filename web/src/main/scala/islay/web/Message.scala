package islay.web

import java.text.MessageFormat
import java.util.{Date, Locale}

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.{NodeSeq, Text, Unparsed, Utility}

import islay.transform.CallingThreadExecutor
import islay.transform.Renderable
import spray.http.HttpHeaders.`Accept-Language`
import spray.http.HttpRequest


object Message {

  /**
   * Returns a Message that will produce an internationalized string when rendered using the given
   * key and arguments. Without specifying a value for `bundle`, messages will be resolved in
   * "messages.props".
   */
  def apply(key: String, args: Any*)
      (implicit bundle: Bundle[Message], locales: Seq[Locale], executor: ExecutionContext) =
    new Message(bundle, key, args: _*)

  implicit object Bundle extends Bundle[Message]("messages")
}


/**
 * A factory that will produce internationalized strings from a properties file when rendered. The
 * properties file is located through `MessageResourceLoader.lookup` using the name from the
 * provided bundle. The property found by the provided key will then be formatted using Java's
 * `MessageFormat` with the provided format arguments. Futures, Options, NodeSeqs, and other
 * Messages may be safely given as format arguments.
 */
class Message(bundle: Bundle[_], key: String, args: Any*)
    (implicit locales: Seq[Locale], executor: ExecutionContext) extends Renderable {


  override def toNodeSeq: Future[NodeSeq] = {

    val messageProps = MessageResourceLoader.lookup(bundle.name, locales)

    import CallingThreadExecutor.Implicit
    messageProps.flatMap { props =>
      props.get(key) match {
        case Some(text) =>
          Future.sequence(args.map(formatArg)) map { args =>
            val s = new MessageFormat(text, props.locale).format(args.toArray)
            Unparsed(s)
          }
        case None =>
          if (args.isEmpty)
            Future.successful(Text("???"+ bundle.name +":"+ key +"???"))
          else
            Future.sequence(args.map(formatArg)) map { args =>
              Unparsed("???"+ bundle.name +":"+ key +"???("+ args.mkString(",") +")")
            }
      }
    }
  }

  private def formatArg(arg: Any): Future[Any] = {
    arg match {
      case null => Future.successful("null")
      case ns: NodeSeq => Future.successful(ns)
      case m: Message => m.toNodeSeq
      case f: Future[_] => f.flatMap(formatArg)
      case None => Future.successful("")
      case Some(a) => formatArg(a)
      case _: Int | _: Long | _: Double | _: Float | _: Short | _: Byte | _: Date => Future.successful(arg)
      case _ => Future.successful(Utility.escape(arg.toString))
    }
  }

  override def toString(): String =
    key +"("+ args.mkString(",") +")"
}


/**
 * A non-internationalized message.
 */
case class FixedMessage(message: NodeSeq) extends Message(null, null)(null, null) {
  override def toNodeSeq: Future[NodeSeq] = Future.successful(message)
  override def toString: String = message.toString
}


class Bundle[A](val name: String)


object Locales {

  /**
   * Extracts the user's preferred locales from the HTTP request.
   */
  def from(request: HttpRequest): Seq[Locale] = {
    /* TODO: support cookie override */
    for (`Accept-Language`(ranges) <- request.headers; range <- ranges)
    yield new Locale(range.primaryTag, range.subTags.mkString("-"))
  }
}