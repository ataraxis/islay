package islay.example

import akka.actor._
import islay.template.{TemplateDirectives, TemplateProcessor}
import spray.routing.{HttpService, Route}
import spray.routing.Directive.pimpApply
import spray.routing.directives.PathMatchers


class ExampleServiceActor extends Actor with ExampleService {

  def actorRefFactory = context

  def receive = runRoute(route)
}

trait ExampleService extends HttpService with TemplateDirectives with PathMatchers {

  implicit val processor = new TemplateProcessor {

  }

  val route: Route = (
    path(Rest)(template)
  )
}