package islay.web

import islay.template.TemplateProcessor
import islay.transform.Transform
import shapeless._
import spray.http.HttpHeader
import spray.routing.{Directive, Directive0, RequestContext, Route}
import spray.routing.directives._


object WebHeaders {

  case class Refresh(timeout: Int, url: String) extends HttpHeader {
    def name = "Refresh"
    def lowercaseName = "refresh"
    def value = s"$timeout; url=$url"
  }

  case class RequestAttributes(attributes: Map[String, Any]) extends HttpHeader {
    def name = "X-Request-Attributes"
    def lowercaseName = "x-request-attributes"
    def value = ""
    def addAttr(name: String, value: Any) = copy(attributes = attributes + (name -> value))
  }

  case class SubmittedValues(values: Map[String, String]) extends HttpHeader {
    def name = "X-Submitted-Values"
    def lowercaseName = "x-submitted-values"
    def value = ""
  }
}

trait WebDirectives {
  import BasicDirectives._
  import MiscDirectives._
  import RespondWithDirectives._
  import WebHeaders._

  def page(t: Transform)(implicit processor: TemplateProcessor): Route =
    new Page { override def transform = t }

  def refresh(url: String): Directive0 = refresh(0, url)

  def refresh(timeout: Int, url: String): Directive0 = respondWithHeader(Refresh(timeout, url))

  def rewritePath(path: String): Directive0 = mapRequestContext { ctx =>
    val q = ctx.request.rawQuery
    val uri = if (q.isEmpty) path else path + "?" + q
    RequestContext(ctx.request.copy(uri = uri).parseUri, ctx.responder, path)
  }

  def flash(content: Message): Directive0 = flash('notice, content)
  def flash(level: Symbol, content: Message): Directive0 = ???

  def requestAttr[T](name: String): Directive[Option[T] :: HNil] = ???
  def requestAttr(name: String, value: Any): Directive0 = ???

  def error(id: String): Directive[Seq[Message] :: HNil] = ???

  def errors: Directive[Seq[Message] :: HNil] = messages('error)
  def warnings: Directive[Seq[Message] :: HNil] = messages('warning)
  def notices: Directive[Seq[Message] :: HNil] = messages('notice)
  def messages(level: Symbol): Directive[Seq[Message] :: HNil] = ???
}

object WebDirectives extends WebDirectives