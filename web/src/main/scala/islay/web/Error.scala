package islay.web

import java.util.Locale

import scala.annotation.{implicitNotFound, tailrec}
import scala.concurrent.{ExecutionContext, Future}
import scala.xml.{NodeSeq, Text}

import islay.transform.CallingThreadExecutor
import islay.web.components.FieldName


object Error {

  implicit object Bundle extends Bundle[Error]("errors")

  /**
   * Creates an error that will produce an internationalized message with the given key and format
   * arguments. The message will implicitly have the field name and value of the associated
   * component passed as its first two format arguments (resolved using the implicit FieldName
   * bundle).
   */
  def apply(key: String, args: Any*)
      (implicit errorBundle: Bundle[Error], fieldBundle: Bundle[FieldName], locales: Seq[Locale], executor: ExecutionContext): Error = {
    new Error(new I18nProtoMessage(key, args.toSeq, errorBundle, fieldBundle, locales, executor))
  }

  /**
   * Creates an error with a given fixed message value.
   */
  def apply(message: NodeSeq): Error = {
    new Error(new FixedProtoMessage(message))
  }

  /**
   * Creates an error with a given fixed message value.
   */
  def text(message: String): Error = {
    apply(Text(message))
  }

  val Empty = new Error(new FixedProtoMessage(NodeSeq.Empty), Future.successful(true))


  private sealed abstract class ProtoMessage {
    def build: Message
    def build(fieldName: String, fieldValue: String): FieldError
  }

  private class I18nProtoMessage(
      key: String,
      args: Seq[Any],
      implicit val errorBundle: Bundle[Error],
      implicit val fieldNameBundle: Bundle[FieldName],
      implicit val locales: Seq[Locale],
      implicit val executor: ExecutionContext
  ) extends ProtoMessage {
    def build: Message = new Message(errorBundle, key, args: _*)
    def build(name: String, value: String): FieldError =
      new Message(errorBundle, key, (FieldName(name) +: value +: args): _*) with FieldError {
        override def fieldName = name
      }
  }

  private class FixedProtoMessage(message: NodeSeq) extends ProtoMessage {
    def build: Message = new FixedMessage(message)
    def build(name: String, value: String): FieldError = new FixedMessage(message) with FieldError {
      override def fieldName = name
    }
  }
}

trait FieldError extends Message {
  def fieldName: String
}

/**
 * A builder for field validation messages returned from component validation callbacks.
 */
class Error private (
    private val message: Error.ProtoMessage,
    private val isFailure: Future[Boolean] = Future.successful(true),
    private val next: Error = Error.Empty
) {

  /**
   * Returns a FieldError message for each failure (where `when` was given `true`) in the chain.
   */
  def fieldErrors(name: String, value: String): Future[Seq[FieldError]] = buildMessages(_.build(name, value))

  /**
   * Returns a Message for each failure (where `when` was given `true`) in the chain.
   */
  def errors: Future[Seq[Message]] = buildMessages(_.build)

  private def buildMessages[A <: Message](f: Error.ProtoMessage => A): Future[Seq[A]] = {
    import CallingThreadExecutor.Implicit
    val fs = toSeq map { e =>
      e.isFailure map { b =>
        (b, e.message)
      }
    }
    Future.sequence(fs) map { ms =>
      ms collect {
        case (true, message) => f(message)
      }
    }
  }

  private def toSeq: Seq[Error] = {
    @tailrec def go(err: Error, acc: List[Error]): List[Error] = {
      if (err == Error.Empty) acc
      else go(err.next, err :: acc)
    }
    go(this, Nil)
  }

  /**
   * Appends another error to the chain of accumulated errors.
   */
  def +(other: Error): Error =
    new Error(other.message, other.isFailure, this)

  /**
   * Filters this error based on a condition. If the condition is false then a failure message
   * will not be produced.
   */
  def when(condition: Boolean): Error =
    new Error(message, Future.successful(condition), next)

  /**
   * Filters this error based on a future condition. If the condition is false then a failure
   * message will not be produced.
   */
  def when(condition: Future[Boolean]): Error =
    new Error(message, condition, next)

  /**
   * Filters this error based on the conjunction of the current failure status and the provided
   * condition. If either the current failure status or the new condition is false then a failure
   * message will not be produced.
   */
  def and(condition: Boolean): Error =
    new Error(message, isFailure.map(_ && condition)(CallingThreadExecutor), next)

  /**
   * Filters this error based on the conjunction of the current failure status and a future
   * condition. If either the current failure status or the future condition is false then a
   * failure message will not be produced.
   */
  def and(condition: Future[Boolean]): Error = {
    import CallingThreadExecutor.Implicit
    val conjunction = for {
      first <- isFailure
      second <- condition
    } yield first && second
    new Error(message, conjunction, next)
  }
}