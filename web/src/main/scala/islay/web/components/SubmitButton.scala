package islay.web.components

import scala.concurrent.Future
import scala.xml.{Attribute, Elem, NodeSeq, Null, Text}

import islay.web.SubmittedValue
import spray.routing.Route


object SubmitButton {
  def apply(onSubmit: => Route): SubmitButton = new SubmitButton(onSubmit)
}


class SubmitButton(onSubmit: => Route) extends Component {

  @volatile private var _name: String = null

  def name = _name
  private[islay] def complete(name: String, value: Option[SubmittedValue]) {
    _name = name
  }

  def route: Route = onSubmit

  def bindTo(elem: Elem): Future[NodeSeq] = Future successful {

    if (elem.label == "button") {
      val attributes =
        elem.attributes append
          Attribute("type", Text("submit"),
          Attribute("name", Text(name),
          Null))

      <button/> % attributes
    }
    else {
      val attributes =
        elem.attributes append
          Attribute("type", Text("submit"),
          Attribute("name", Text(name),
          Null))

      <input/> % attributes
    }
  }
}