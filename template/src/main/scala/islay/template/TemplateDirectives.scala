package islay.template

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.xml.NodeSeq

import akka.actor._
import shapeless._
import spray.http.HttpRequest
import spray.routing.{Directive, RequestContext, Route}


trait TemplateDirectives {

  def completeTemplate(nodes: NodeSeq)(implicit processor: TemplateProcessor): Route = completeTemplate(nodes, _)

  def completeTemplate(nodes: NodeSeq, ctx: RequestContext)(implicit processor: TemplateProcessor) {
    if (ctx.request.headers.contains(TemplateMarker))
      ctx.responder ! nodes
    else
      ctx.responder ! processor.format(nodes)
  }

  def completeTemplate(future: Future[NodeSeq])
      (implicit executor: ExecutionContext, processor: TemplateProcessor): Route = completeTemplate(future, _)

  def completeTemplate(future: Future[NodeSeq], ctx: RequestContext)
      (implicit executor: ExecutionContext, processor: TemplateProcessor) {
    future onComplete {
      case Success(nodes) => completeTemplate(nodes, ctx)
      case Failure(ex) => ctx.failWith(ex)
      case m => ctx.failWith(new Exception("Invalid template completion: "+ m))
    }
  }

  def template(path: String)(implicit executor: ExecutionContext, processor: TemplateProcessor): Route =
    template(path, _)

  def template(path: String, ctx: RequestContext)
      (implicit executor: ExecutionContext, processor: TemplateProcessor) {
    val request = HttpRequest(uri = path).parseUri
    for {
      nodes <- processor.lookup(request)
      expanded <- processor.expand(nodes, ctx)
    } completeTemplate(expanded, ctx)
  }

  def originalContext: Directive[RequestContext :: HNil] = ???
}

object TemplateDirectives extends TemplateDirectives