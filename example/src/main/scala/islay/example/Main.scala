package islay.example

import akka.io.IO
import spray.can.Http
import spray.can.Http.Bind


object Main extends App with Modules {

  IO(Http) ! Bind(service, interface = "localhost", port = 8080)
}