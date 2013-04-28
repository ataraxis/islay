package islay.template

import islay.transform.Transform
import spray.routing.{RequestContext, Route}
import islay.transform.CallingThreadExecutor


trait TemplateBinding extends Route {

  implicit def processor: TemplateProcessor

  def transform: Transform

  def apply(context: RequestContext) {
    import TemplateDirectives._
    import CallingThreadExecutor.Implicit

    for {
      template <- processor.lookup(context.request.path)
      nodes <- transform(template)
      expanded <- processor.expand(nodes, context)
    } completeTemplate(expanded, context)
  }
}