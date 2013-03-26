package islay.transform

import scala.xml.NodeSeq
import scala.xml.Elem
import scala.concurrent.Future


trait Bindable {
  def bindTo(elem: Elem): Future[NodeSeq]
}