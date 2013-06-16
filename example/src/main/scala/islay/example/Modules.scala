package islay.example

import scala.concurrent.ExecutionContext

import akka.actor.{ActorSystem, Props}
import spray.routing.HttpService


/**
 * Here's where we bake our cake.
 */
trait Modules
  extends HttpService
  with Routes
  with TemplateRoutes
  with TemplateProcessorModule {

  implicit val system = ActorSystem("example")
  override val actorRefFactory = system

  override implicit val executor: ExecutionContext = system.dispatcher
  override implicit val templateProcessor = new ExampleTemplateProcessor

  val service = system.actorOf(Props(new ExampleService(route)))
}