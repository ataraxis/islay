package islay.web

import scala.util.{Failure, Success}

import spray.http.HttpMethods
import spray.routing.RequestContext


trait FormPage extends Page {

  val form: Form


  override def apply(context: RequestContext): Unit = {
    super.init(context)
    form.complete()
    context.request.method match {
      case HttpMethods.GET | HttpMethods.HEAD =>
      case _ =>
        /* TODO: check CSRF token */
    }

    implicit val ec = processor.executor
    form.process onComplete {
      case Failure(ex) =>
        context.failWith(ex)
      case Success(Right(route)) =>
        route.apply(context)
      case Success(Left(errors)) =>
        val ctx = context.withRequestMapped { request =>
          WebContext.from(request).addMessages('error, errors).withinRequest
        }
        super.complete(ctx)
    }
  }
}