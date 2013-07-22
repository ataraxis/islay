package islay.example

import islay.example.pages.FeedbackSnippetModule
import islay.template.TemplateProcessor
import spray.routing.Route


trait TemplateProcessorModule extends ExecutorModule
with FeedbackSnippetModule {

  implicit val templateProcessor: TemplateProcessor

  class ExampleTemplateProcessor extends TemplateProcessor {
    override val route: Route = (
      path("header")(feedbackSnippet) ~
      super.route
    )
  }
}