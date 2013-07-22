package islay.web.components

import java.util.Locale

import scala.concurrent.{ExecutionContext, Future}

import islay.web.{Error, FieldError, Message, SubmittedValue}


abstract class ValueComponent[A](implicit
    converter: FieldConverter[A],
    locales: Seq[Locale],
    executor: ExecutionContext
) extends Component {

  import FieldConverter._

  @volatile private var _completedValue: Value = _
  override private[islay] def complete(name: String, value: Option[SubmittedValue]) {
    _completedValue = new Value(name, value)
  }
  private[this] def completedValue = {
    val v = _completedValue
    if (v == null)
      throw new IllegalStateException("Uncompleted value")
    else
      v
  }

  def default: A
  def required: Boolean = false

  override def name: String = completedValue.name
  def value: A = completedValue.converted
  def valueAsString: String = completedValue.asString

  def validator: (A => Error)
  def errors: Future[Seq[FieldError]] = completedValue.errors


  protected class Value(val name: String, val submitted: Option[SubmittedValue]) {

    def requiredError = new Message(Error.Bundle, "required", FieldName(name)) with FieldError {
      override def fieldName = name
    }

    def requiredTriple =
      ("", default, Future.successful(requiredError :: Nil))

    val (asString, converted, errors) = submitted match {
      case None if required =>
        requiredTriple
      case None =>
        (converter.toString(default), default, Future.successful(Nil))
      case Some(value) =>
        val stringValue = value.asString
        if (stringValue.isEmpty && required)
          requiredTriple
        else {
          val (converted, errors) = converter.fromSubmitted(value) match {
            case Right(v) => (v, validator(v))
            case Left(es) => (default, es)
          }
          (stringValue, converted, errors.failureMessages(name, stringValue))
        }
    }
  }
}