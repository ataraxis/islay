package islay.example


object Main extends App with Modules {
  newHttpServer(service) ! Bind(interface = "localhost", port = 8080)
}