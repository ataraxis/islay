package islay.example

import akka.actor._
import islay.template.{TemplateDirectives, TemplateProcessor}
import spray.routing.{HttpService, RequestContext, Route}


class ExampleServiceActor extends Actor with ExampleService {

  def actorRefFactory = context

  def receive = runRoute(route)
}

trait ExampleService extends HttpService with TemplateDirectives {

  implicit val processor = new TemplateProcessor {

  }

  val route: Route = (
    path("foo") { ctx =>
      complete("")
    } ~
    path("foo").apply(foo) ~
    template("index.html") ~
    template("plah")
  )

  def foo(ctx: RequestContext): Route = {
    template("plah")
  }
}