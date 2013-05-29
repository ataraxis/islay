package islay.web

import scala.concurrent.Future
import scala.xml.{Elem, NodeSeq}

import islay.transform.Bindable


class Form(fields: Map[String, Bindable]) extends Bindable {

  override def bindTo(elem: Elem): Future[NodeSeq] = ???
}