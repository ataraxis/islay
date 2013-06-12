package islay.web

import scala.concurrent.Future
import scala.xml.{Elem, NodeSeq}

import islay.transform.Bindable
import islay.transform.CallingThreadExecutor
import islay.web.components.{Component, SubmitButton}
import spray.http.HttpRequest
import spray.routing.Route


abstract class Form(implicit request: HttpRequest) extends Bindable {

  val fields: Map[String, Component]
  private val submittedValues = SubmittedValue.extractAll(request)

  override def bindTo(elem: Elem): Future[NodeSeq] = {
    fields.foldLeft(Future successful elem) { case (acc, (_, component)) =>
      import CallingThreadExecutor.Implicit
      acc flatMap { next =>
        component.selector(next)(component.bindTo).map(nodeSeqToElem)
      }
    }
  }

  private def nodeSeqToElem(ns: NodeSeq): Elem =
    ns.collectFirst { case e: Elem => e }.get


  private[islay] def complete() {
    for ((name, component) <- fields) {
      component.complete(name, submittedValues.get(name))
    }
  }


  def process(): Option[Route] = {
    fields collectFirst {
      case (name, button: SubmitButton) if submittedValues.contains(name) =>
        button.route
    }
  }
}