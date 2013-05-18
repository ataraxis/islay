package islay.example

import islay.template.TemplateProcessor
import spray.routing.{HttpService, Route}


trait TemplateProcessorModule extends HttpService with TemplateRoutes {

  implicit val templateProcessor = new TemplateProcessor {
    override val route: Route = templateRoute ~ super.route
  }
}