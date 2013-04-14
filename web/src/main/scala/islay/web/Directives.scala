package islay.web

import shapeless._
import spray.http.HttpHeader
import spray.routing.{Directive, Directive0, RequestContext}
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
}

trait Directives {
  import BasicDirectives._
  import MiscDirectives._
  import RespondWithDirectives._
  import WebHeaders._

  def refresh(url: String): Directive0 = refresh(0, url)

  def refresh(timeout: Int, url: String): Directive0 = respondWithHeader(Refresh(timeout, url))

  def rewritePath(path: String): Directive0 = mapRequestContext { ctx =>
    val q = ctx.request.rawQuery
    val uri = if (q.isEmpty) path else path + "?" + q
    RequestContext(ctx.request.copy(uri = uri).parseUri, ctx.responder, path)
  }

  def flash(content: String): Directive0

  def requestAttr[T](name: String): Directive[Option[T] :: HNil] = null
  def requestAttr(name: String, value: Any): Directive0 = null
}