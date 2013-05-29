package islay.web

import spray.http.HttpMethods
import spray.routing.RequestContext


trait FormPage extends Page {

  def form: Form

  override def apply(context: RequestContext) {
    context.request.method match {
      case HttpMethods.POST | HttpMethods.PUT =>
        super.init(context)
        ???
        super.bind()
      case _ =>
        super.apply(context)
    }
  }
}