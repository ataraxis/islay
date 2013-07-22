package islay.web

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.{Elem, NodeSeq}

import islay.transform.Bindable
import islay.transform.CallingThreadExecutor
import islay.web.components.{Component, SubmitButton, ValueComponent}
import spray.http.HttpRequest
import spray.routing.Route


/**
 * An HTML form binding which includes all components contained within the form. Binding a form in
 * a transformation automatically binds all its components (using their name attribute).
 */
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

  def fieldErrors: Future[Seq[Message]] = {
    val fs = fields collect {
      case (_, c: ValueComponent[_]) => c.errors
    }
    import CallingThreadExecutor.Implicit
    Future.sequence(fs) map { _.toSeq.flatten }
  }

  /**
   * Returns either the field errors for this form if validation failed or the route returned from
   * the clicked submit button.
   */
  def process(implicit executor: ExecutionContext): Future[Either[Seq[Message], Route]] = {
    fieldErrors map {
      case Nil =>
        fields collectFirst {
          case (name, button: SubmitButton) if submittedValues.contains(name) =>
            Right(button.route)
        } getOrElse {
          Left(Nil)
        }
      case errors =>
        Left(errors)
    }
  }
}