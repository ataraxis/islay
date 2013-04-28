package islay.example

import akka.actor.Props
import spray.can.server.SprayCanHttpServerApp


object Main extends App with SprayCanHttpServerApp {

  val service = system.actorOf(Props[ExampleServiceActor])

  newHttpServer(service) ! Bind(interface = "localhost", port = 8080)
}