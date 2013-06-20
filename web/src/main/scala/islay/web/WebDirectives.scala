package islay.web

import islay.template.TemplateProcessor
import islay.transform.Transform
import shapeless._
import spray.http.{HttpHeader, HttpHeaders, SingletonValueRenderable, StringRendering, Uri}
import spray.routing.{Directive, Directive0, RequestContext, Route, RouteConcatenation}
import spray.routing.directives._
import islay.template.util.Resources
import islay.template.TemplateSettings


object WebHeaders {

  case class Refresh(timeout: Int, url: Uri) extends HttpHeader with SingletonValueRenderable {
    def name = "Refresh"
    def lowercaseName = "refresh"
    override def value = s"$timeout; url=${url.render(new StringRendering)}"
  }

  case class RequestAttributes(attributes: Map[String, Any]) extends HttpHeader with SingletonValueRenderable {
    def name = "X-Request-Attributes"
    def lowercaseName = "x-request-attributes"
    def addAttr(name: String, value: Any) = copy(attributes = attributes + (name -> value))
  }
}

trait WebDirectives {
  import BasicDirectives._
  import MiscDirectives._
  import PathDirectives._
  import RespondWithDirectives._
  import WebHeaders._

  def refresh(url: String): Directive0 = refresh(0, url)

  def refresh(timeout: Int, url: String): Directive0 = respondWithHeader(Refresh(timeout, url))

  def rewritePath(path: String): Directive0 = mapRequestContext { ctx =>
    val absolutePath = if (path startsWith "/") path else "/"+ path
    val uri = ctx.request.uri.copy(path = Uri.Path(absolutePath))
    RequestContext(ctx.request.copy(uri = uri), ctx.responder, uri.path)
  }

  def flash(content: Message): Directive0 = flash('notice, content)

  def flash(level: Symbol, content: Message): Directive0 = mapHttpResponseHeaders { headers =>
    headers map {
      case Refresh(timeout, url) =>
        Refresh(timeout, flashUrl(url, level, content))
      case HttpHeaders.Location(url) =>
        HttpHeaders.Location(flashUrl(url, level, content))
      case other =>
        other
    }
  }

  private def flashUrl(url: Uri, level: Symbol, content: Message) =
    url.copy(query = Uri.Query.Cons("!flash", encodeFlash(level, content), url.query))

  private def encodeFlash(level: Symbol, content: Message) = content.toString

  def requestAttr[T](name: String): Directive[Option[T] :: HNil] = ???
  def requestAttr(name: String, value: Any): Directive0 = ???

  def error(id: String): Directive[Seq[Message] :: HNil] = ???

  def errors: Directive[Seq[Message] :: HNil] = messages('error)
  def warnings: Directive[Seq[Message] :: HNil] = messages('warning)
  def notices: Directive[Seq[Message] :: HNil] = messages('notice)
  def messages(level: Symbol): Directive[Seq[Message] :: HNil] = ???
}

object WebDirectives extends WebDirectives