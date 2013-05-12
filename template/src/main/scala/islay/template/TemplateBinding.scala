package islay.template

import islay.transform.CallingThreadExecutor
import islay.transform.Transform
import spray.routing.{RequestContext, Route}


trait TemplateBinding extends Route {

  implicit def processor: TemplateProcessor

  def transform: Transform

  def apply(context: RequestContext) {
    import TemplateDirectives._
    import CallingThreadExecutor.Implicit

    val f = for {
      template <- processor.lookup(context.request)
      nodes <- transform(template)
      expanded <- processor.expand(nodes, context)
    } yield expanded

    complete(f, context)
  }
}