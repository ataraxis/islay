package islay.web

import java.text.MessageFormat
import java.util.{Date, Locale}

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.{NodeSeq, Text, Unparsed, Utility}

import islay.transform.CallingThreadExecutor
import islay.transform.Renderable
import spray.http.HttpHeaders.`Accept-Language`
import spray.http.HttpRequest


case class Message(key: String, args: Any*)
    (implicit bundle: Bundle, locales: Seq[Locale], executor: ExecutionContext) extends Renderable {


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
            Future.successful(Text("???"+ key +"???"))
          else
            Future.sequence(args.map(formatArg)) map { args =>
              Unparsed("???"+ key +"???("+ args.mkString(",") +")")
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
}


object Bundle {
  implicit val messages = Bundle("messages")
}

case class Bundle(name: String)


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