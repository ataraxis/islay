package islay.web.components

import java.util.Locale

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

import islay.web.{Message, SubmittedValue}
import spray.http.HttpRequest


abstract class ValueComponent[A](implicit
    converter: FieldConverter[A],
    request: HttpRequest,
    locales: Seq[Locale],
    executor: ExecutionContext
) extends Component {

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
  def value: A = completedValue.converted.get
  def valueAsString: String = completedValue.asString


  protected class Value(val name: String, val submitted: Option[SubmittedValue]) {

    val asString: String = submitted.fold(converter.toString(default))(_.asString)
    val converted: Try[A] = submitted.fold(Try(default))(converter.fromSubmitted)

    val errors: Seq[Message] = converted match {
      case Success(value) =>
        // do validation
        Nil
      case Failure(ex) =>
        Message("conversion.error.unknown", ex.getMessage) :: Nil
    }

    def validate: Message = {
//      if (submittedValue.isEmpty)
//        null
      ???
    }
  }
}