package islay.template

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

import shapeless._
import spray.routing.{Directive, RequestContext, Route}


trait TemplateDirectives {

  def complete(result: TemplateResult)(implicit processor: TemplateProcessor): Route = complete(result, _)

  def complete(result: TemplateResult, ctx: RequestContext)(implicit processor: TemplateProcessor) {
    if (ctx.request.headers.contains(IncludeMarker))
      ctx.responder ! result
    else
      ctx.responder ! processor.format(result)
  }

  def complete(future: Future[TemplateResult])
      (implicit executor: ExecutionContext, processor: TemplateProcessor): Route = complete(future, _)

  def complete(future: Future[TemplateResult], ctx: RequestContext)
      (implicit executor: ExecutionContext, processor: TemplateProcessor) {
    future onComplete {
      case Success(result) => complete(result, ctx)
      case Failure(ex) => ctx.failWith(ex)
    }
  }

  def template(path: String)(implicit executor: ExecutionContext, processor: TemplateProcessor): Route =
    template(path, _)

  def template(path: String, ctx: RequestContext)
      (implicit executor: ExecutionContext, processor: TemplateProcessor) {
    val request = ctx.request.copy(uri = path)
    val f = for {
      nodes <- processor.lookup(request)
      expanded <- processor.expand(nodes, ctx)
    } yield expanded
    complete(f, ctx)
  }
}

object TemplateDirectives extends TemplateDirectives