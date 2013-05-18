package islay.example

import akka.actor._
import islay.template.TemplateProcessor
import spray.can.server.SprayCanHttpServerApp
import spray.routing.{HttpService, HttpServiceActor, Route}

/**
 * Here's where we bake our cake.
 */
trait Modules
  extends SprayCanHttpServerApp
  with HttpService
  with Routes
  with TemplateRoutes
  with TemplateProcessorModule {


  override def actorRefFactory = system

  val service = system.actorOf(Props(HttpServiceActor(route)))
}