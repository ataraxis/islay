package islay.web.components

import java.util.Locale

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

import islay.transform.Bindable
import islay.web.{Message, WebHeaders}
import spray.http.HttpRequest


abstract class Component[A](implicit
    converter: TextConverter[A],
    request: HttpRequest,
    locales: Seq[Locale],
    executor: ExecutionContext
) extends Bindable {

  @volatile private var _completedValue: Value = _
  private[islay] def complete(name: String) {
    _completedValue = new Value(name)
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

  def id = maybeId getOrElse name
  protected def maybeId: Option[String] = None


  def name: String = completedValue.name
  def submittedValue = completedValue.submitted
  def value: A = completedValue.converted.get

  private def submittedValues: Map[String, String] = {
    request.headers collectFirst {
      case WebHeaders.SubmittedValues(values) => values
    } getOrElse {
      Map.empty
    }
  }

  protected class Value(val name: String) {

    val submitted = submittedValues.getOrElse(name, converter.toString(default))
    val converted = converter.fromString(submittedValue)

    val errors: Seq[Message] = converted match {
      case Success(value) =>
        // do validation
        Nil
      case Failure(ex) =>
        Message("conversion.error.unknown", ex.getMessage) :: Nil
    }

    def validate: Message = {
      if (submittedValue.isEmpty)
        null
      ???
    }
  }
}