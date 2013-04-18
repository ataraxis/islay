package islay.template

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.xml.NodeSeq

import akka.actor._
import shapeless._
import spray.routing.{Directive, RequestContext, Route}


trait TemplateDirectives {

  def completeTemplate(nodes: NodeSeq)(implicit processor: TemplateProcessor): Route = completeTemplate(nodes, _)

  def completeTemplate(nodes: NodeSeq, ctx: RequestContext)(implicit processor: TemplateProcessor) {
    ctx.responder match {
      case r: TemplateResponder => r ! nodes
      case r => r ! processor.format(nodes)
    }
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

  def template(path: String) {
    ???
  }

  def originalContext: Directive[RequestContext :: HNil] = ???
}

object TemplateDirectives extends TemplateDirectives