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

  def isWithoutFieldErrors: Future[Boolean] = {
    fieldErrors.map(_.isEmpty)(CallingThreadExecutor)
  }

  private def errors: Future[Seq[Message]] = {
    import CallingThreadExecutor.Implicit
    for {
      fes <- fieldErrors
      es <- validate.errors
    } yield fes ++ es
  }

  /**
   * User-overrideable method to produce additional form errors not associated with any field. To
   * only produce errors if there are no field errors, `isWithoutFieldErrors` can be used:
   *
   * {{{
   * Error.text("Login failed") when (isWithoutFieldErrors) and {
   *   authenticate(username.value, password.value).isEmpty
   * }
   * }}}
   */
  def validate: Error = Error.Empty

  /**
   * Returns either the errors for this form if validation failed or the route returned from the
   * clicked submit button.
   */
  def process(implicit executor: ExecutionContext): Future[Either[Seq[Message], Route]] = {
    val submitButton = fields collectFirst {
      case (name, button: SubmitButton) if submittedValues.contains(name) =>
        button
    }
    submitButton match {
      case None => Future successful Left(Nil)
      case Some(button) =>
        errors map {
          case Nil => Right(button.route)
          case errors => Left(errors)
        }
    }
  }
}