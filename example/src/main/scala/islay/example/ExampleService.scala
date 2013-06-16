package islay.example

import spray.routing.HttpServiceActor
import spray.routing.Route


class ExampleService(route: Route) extends HttpServiceActor {

  def receive = runRoute(route)
}