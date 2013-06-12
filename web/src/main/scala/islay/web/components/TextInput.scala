package islay.web.components

import java.util.Locale

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.{Attribute, Elem, NodeSeq, Null, Text}

import spray.http.HttpRequest


object TextInput {

  def apply[A](default: A, required: Boolean = false, validator: (A => Unit) = { _: A => () })
      (implicit tc: FieldConverter[A], r: HttpRequest, ls: Seq[Locale], e: ExecutionContext): TextInput[A] = {
    new TextInput(default, required, validator)
  }
}

class TextInput[A] private (
    override val default: A,
    required: Boolean,
    validator: (A => Unit)
)(implicit
    tc: FieldConverter[A],
    r: HttpRequest,
    ls: Seq[Locale],
    executor: ExecutionContext
) extends ValueComponent[A] {


  override def bindTo(elem: Elem): Future[NodeSeq] = Future successful {

    val attributes =
      elem.attributes append
        Attribute("type", Text("text"),
        Attribute("name", Text(name),
        Attribute("value", Text(valueAsString),
        Null)))

    <input/> % attributes
  }
}