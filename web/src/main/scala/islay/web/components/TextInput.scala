package islay.web.components

import java.util.Locale

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.{Attribute, Elem, NodeSeq, Null, Text}

import spray.http.HttpRequest


object TextInput {

  def apply[A](default: A, id: String = null, required: Boolean = false, validator: (A => Unit) = { _: A => () })
      (implicit tc: TextConverter[A], r: HttpRequest, ls: Seq[Locale], e: ExecutionContext): Component[A] = {
    new TextInput(default, Option(id), required, validator)
  }
}

class TextInput[A] private (
    override val default: A,
    override val maybeId: Option[String],
    required: Boolean,
    validator: (A => Unit)
)(implicit
    tc: TextConverter[A],
    r: HttpRequest,
    ls: Seq[Locale],
    executor: ExecutionContext
) extends Component[A] {


  override def bindTo(elem: Elem): Future[NodeSeq] = Future successful {

    val attributes =
      elem.attributes append
        Attribute("type", Text("text"),
        Attribute("id", Text(id),
        Attribute("name", Text(name),
        Attribute("value", Text(submittedValue),
        Null))))

    <input/> % attributes
  }
}