package islay.example

import islay.template.TemplateProcessor
import spray.routing.{HttpService, Route}


trait TemplateProcessorModule extends ExecutorModule with TemplateRoutes {

  implicit val templateProcessor: TemplateProcessor

  class ExampleTemplateProcessor extends TemplateProcessor {
    override val route: Route = templateRoute ~ super.route
  }
}