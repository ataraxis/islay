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

  /**
   * Returns a Message deserialized from the given string that was serialized using
   * `Message.serialize`.
   */
  def deserialize(value: String)(implicit locales: Seq[Locale], executor: ExecutionContext): (Symbol, Message) = {
    split(value) match {
      case level +: bundle +: key +: args =>
        val safeBundle = bundle.replaceAll("[^\\w\\d-.]", "")
        Symbol(level) -> new Message(new Bundle(safeBundle), key, args.map(deserializeArg): _*)
      case _ =>
        'error -> FixedMessage(<span>Invalid flash parameter: <cite>{value}</cite></span>)
    }
  }

  private def split(value: String): Seq[String] = {
    val sb = new StringBuilder
    var seq = Vector[String]()
    var escaped = false
    var i = 0
    while (i < value.length) {
      value.charAt(i) match {
        case '\\' if !escaped =>
          escaped = true
        case c if escaped =>
          sb.append(c)
          escaped = false
        case '~' =>
          seq :+= sb.toString
          sb.clear()
        case c =>
          sb.append(c)
      }
      i += 1
    }
    if (!sb.isEmpty)
      seq :+= sb.toString
    seq
  }

  private val ArgPattern = "([id]?):(.*)".r

  private def deserializeArg(arg: Any): Any = arg match {
    case ArgPattern("d", value) => value.toDouble
    case ArgPattern("i", value) => value.toLong
    case ArgPattern("", value) => value
    case _ => "Cheesewiz"
  }
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

  /**
   * Returns a serialized representation that is safe to be passed between client and server (and
   * therefore suitable for flash storage).
   */
  def serialize(level: Symbol): Future[String] = {
    import CallingThreadExecutor.Implicit
    Future.sequence(args.map(serializeArg)) map { args =>
      level.name +"~"+ bundle.name +"~"+ key +"~"+ args.mkString("~")
    }
  }

  private def serializeArg(arg: Any): Future[String] = {
    import CallingThreadExecutor.Implicit
    arg match {
      case null => Future.successful(":null")
      case None => Future.successful(":")
      case Some(a) => serializeArg(a)
      case f: Future[_] => f.flatMap(serializeArg)
      case _: Int | _: Long | _: Short | _: Byte => Future.successful("i:"+ arg)
      case _: Float | _: Double => Future.successful("d:"+ arg)
      case a => Future.successful(":"+ escapeForSerialization(a.toString))
    }
  }

  private def escapeForSerialization(arg: String): String = {
    Utility.escape(arg).replaceAll("([~\\\\])", "\\\\$1")
  }

  override def toString(): String =
    if (args.isEmpty) bundle +":"+ key
    else bundle +":"+ key +"("+ args.mkString(",") +")"
}


/**
 * A non-internationalized message.
 */
case class FixedMessage(message: NodeSeq) extends Message(null, null)(null, null) {
  override def toNodeSeq: Future[NodeSeq] = Future.successful(message)
  override def toString: String = message.toString
}


class Bundle[A](val name: String) {
  override def toString = name
}


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