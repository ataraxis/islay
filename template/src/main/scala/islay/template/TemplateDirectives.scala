package islay.template

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.xml.NodeSeq

import akka.actor._
import shapeless._
import spray.routing.{Directive, RequestContext, Route}


trait TemplateDirectives {

  def completeTemplate(nodes: NodeSeq): Route = completeTemplate(nodes, _)

  def completeTemplate(nodes: NodeSeq, ctx: RequestContext) {
    ctx.responder ! nodes
  }

  def completeTemplate(future: Future[NodeSeq])(implicit executor: ExecutionContext): Route = { ctx =>
    future onComplete {
      case Success(nodes) => ctx.responder ! nodes
      case Failure(ex) => ctx.failWith(ex)
    }
  }

  def template(path: String) {
    ???
  }

  def originalContext: Directive[RequestContext :: HNil] = ???
}

object TemplateDirectives extends TemplateDirectives