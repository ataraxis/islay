package islay.web

import spray.http.HttpMethods
import spray.routing.RequestContext


trait FormPage extends Page {

  val form: Form


  override def apply(context: RequestContext) {
    super.init(context)
    form.complete()
    context.request.method match {
      case HttpMethods.GET | HttpMethods.HEAD =>
      case _ =>
        /* TODO: check CSRF token */
    }
    form.process() match {
      case Some(route) => route.apply(context)
      case None => super.bind()
    }
  }
}