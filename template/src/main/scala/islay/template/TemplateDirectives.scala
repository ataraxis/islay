package islay.template

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

import akka.actor._
import shapeless._
import spray.http.HttpRequest
import spray.routing.{Directive, RequestContext, Route}
import spray.routing.directives.BasicDirectives


trait TemplateDirectives {

  import BasicDirectives._

  def complete(result: TemplateResult)(implicit processor: TemplateProcessor): Route = complete(result, _)

  def complete(result: TemplateResult, ctx: RequestContext)(implicit processor: TemplateProcessor): Unit = {
    if (ctx.request.headers.exists(_.isInstanceOf[ParentRequest]))
      ctx.responder ! result
    else
      ctx.responder ! processor.format(result)
  }

  def complete(future: Future[TemplateResult])
      (implicit executor: ExecutionContext, processor: TemplateProcessor): Route = complete(future, _)

  def complete(future: Future[TemplateResult], ctx: RequestContext)
      (implicit executor: ExecutionContext, processor: TemplateProcessor): Unit = {
    future onComplete {
      case Success(result) => complete(result, ctx)
      case Failure(ex) => ctx.failWith(ex)
    }
  }

  def template(path: String)(implicit executor: ExecutionContext, processor: TemplateProcessor): Route =
    template(path, _)

  def template(path: String, ctx: RequestContext)
      (implicit executor: ExecutionContext, processor: TemplateProcessor): Unit = {
    val request = ctx.request.copy(uri = path)
    val f = for {
      nodes <- processor.lookup(request)
      expanded <- processor.expand(nodes, ctx)
    } yield expanded
    complete(f, ctx)
  }

  /**
   * Extracts the original HTTP request from within a template request.
   */
  def originalRequest: Directive[HttpRequest :: HNil] = extract { ctx =>
    def parent(req: HttpRequest) = req.headers.collectFirst { case ParentRequest(r) => r }
    def go(req: HttpRequest): HttpRequest = parent(req).fold(req)(go)
    go(ctx.request)
  }
}

object TemplateDirectives extends TemplateDirectives