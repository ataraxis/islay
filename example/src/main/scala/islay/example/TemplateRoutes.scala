package islay.example

import spray.routing.Route

trait TemplateRoutes {

  def templateRoute: Route = _.reject()
}