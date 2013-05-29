package islay.template

import TemplateDirectives.complete
import islay.transform.CallingThreadExecutor
import islay.transform.Transform
import spray.http.HttpMethods
import spray.routing.{RequestContext, Route}


trait TemplateBinding extends Route with DelayedInit {

  @volatile private[this] var context: RequestContext = null
  private[islay] def setContext(context: RequestContext) {
    if (this.context != null)
      throw new IllegalStateException("Template bindings cannot be shared across requests. Try using the dynamic directive.")
    this.context = context
  }

  implicit def request = {
    val c = context
    if (c == null)
      throw new IllegalStateException("Context not yet available")
    else
      c.request
  }

  @volatile private[this] var body: () => Unit = _

  override def delayedInit(body: => Unit) {
    this.body = body _
  }

  /**
   * The processor that should be used to resolve this binding's template.
   */
  implicit def processor: TemplateProcessor

  /**
   * The transformation that binds data to this template.
   */
  def transform: Transform

  def apply(context: RequestContext) {
    context.request.method match {
      case HttpMethods.GET =>
        init(context)
        bind()
      case _ =>
        context.reject()
    }
  }

  private[islay] def init(context: RequestContext) {
    setContext(context)
    val b = body
    if (b != null) b()
  }

  private[islay] def bind() {
    import TemplateDirectives._
    import CallingThreadExecutor.Implicit

    val context = this.context

    val f = for {
      template <- processor.lookup(context.request)
      nodes <- transform(template)
      expanded <- processor.expand(nodes, context)
    } yield expanded

    complete(f, context)
  }
}