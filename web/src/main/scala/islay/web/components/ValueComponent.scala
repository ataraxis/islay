package islay.web.components

import java.util.Locale

import scala.concurrent.{ExecutionContext, Future}

import islay.transform.CallingThreadExecutor
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

  /**
   * The value this component has before a value has been submitted.
   */
  def default: A

  /**
   * True if this component will fail validation when an empty value is submitted.
   */
  def required: Boolean = false

  /**
   * The form parameter name for this field.
   */
  override def name: String = completedValue.name

  /**
   * The converted value for this field, or the default value if the submitted value could not be
   * converted.
   */
  def value: A = completedValue.converted

  /**
   * The value as it was submitted in the HTTP request, or the empty string if no value has been
   * submitted.
   */
  def valueAsString: String = completedValue.asString

  /**
   * The function called to validate the converted value.
   */
  def validator: (A => Error)

  /**
   * The field errors collected from validation, where the FieldError's `fieldName` is the name of
   * this component.
   */
  def errors: Future[Seq[FieldError]] = completedValue.errors

  /**
   * Returns `true` when there are no field errors. Useful with validators for other components
   * when you don't want an error to manifest itself unless this component is error-free:
   *
   * {{{
   * Error.text("Passwords don't match").
   *   when (password1.isWithoutErrors).
   *   and (password2.isWithoutErrors).
   *   and (password1.value != password2.value)
   * }}}
   */
  def isWithoutErrors: Future[Boolean] = errors.map(_.isEmpty)(CallingThreadExecutor)


  protected class Value(val name: String, val submitted: Option[SubmittedValue]) {

    def requiredError = new Message(Error.Bundle, "required", FieldName(name)) with FieldError {
      override def fieldName = name
    }

    val (asString, converted, errors) = submitted match {
      case None =>
        (converter.toString(default), default, Future.successful(Nil))
      case Some(value) =>
        val stringValue = value.asString
        if (stringValue.isEmpty && required)
          ("", default, Future.successful(requiredError :: Nil))
        else {
          val (converted, errors) = converter.fromSubmitted(value) match {
            case Right(v) => (v, validator(v))
            case Left(es) => (default, es)
          }
          (stringValue, converted, errors.fieldErrors(name, stringValue))
        }
    }
  }
}