package islay.web

import shapeless._
import spray.http.{HttpHeader, HttpHeaders, HttpRequest, Rendering, SingletonValueRenderable, Uri}
import spray.routing.{Directive, Directive0, RequestContext, Route, RouteConcatenation}
import spray.routing.directives._


object WebHeaders {

  case class Refresh(timeout: Int, url: Uri) extends HttpHeader {
    def name = "Refresh"
    def lowercaseName = "refresh"
    def value = s"$timeout; url=${url}"
    def render[R <: Rendering](r: R): r.type = r ~~ name ~~ ':' ~~ ' ' ~~ value
  }

  case class RequestAttributes(attributes: Map[String, Any]) extends HttpHeader with SingletonValueRenderable {
    def name = "X-Request-Attributes"
    def lowercaseName = "x-request-attributes"
    def addAttr(name: String, value: Any) = copy(attributes = attributes + (name -> value))
  }

  case class Messages(messages: Map[Symbol, Seq[Message]]) extends HttpHeader with SingletonValueRenderable {
    def name = "X-Messages"
    def lowercaseName = "x-messages"
  }

  def addMessages(request: HttpRequest, level: Symbol, msgs: Seq[Message]): HttpRequest = {
    val pair = (level, msgs)
    var found = false
    val updatedHeaders = request.headers map {
      case h @ Messages(ms) =>
        found = true
        h.copy(messages = append(ms, level, msgs))
      case h =>
        h
    }
    val hs =
      if (found) updatedHeaders
      else Messages(Map(pair)) +: request.headers
    request.copy(headers = hs)
  }

  private def append(ms: Map[Symbol, Seq[Message]], level: Symbol, msgs: Seq[Message]) = {
    val seq = ms.get(level) match {
      case Some(seq) => msgs ++: seq
      case None => msgs
    }
    ms + ((level, seq))
  }

  def findMessages(request: HttpRequest, level: Symbol): Seq[Message] = {
    request.headers collectFirst {
      case Messages(ms) => ms.getOrElse(level, Nil)
    } getOrElse Nil
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

  def error(msg: Message): Directive0 = message('error, msg)
  def warning(msg: Message): Directive0 = message('warning, msg)
  def notice(msg: Message): Directive0 = message('notice, msg)
  def message(level: Symbol, msg: Message): Directive0 = messages(level, List(msg))

  def messages(level: Symbol, msgs: Seq[Message]): Directive0 = mapRequest { addMessages(_, level, msgs) }

  def error(id: String): Directive[Seq[Message] :: HNil] = extract { ctx =>
    findMessages(ctx.request, 'error) collect {
      case m: FieldError if m.fieldName == id => m
    }
  }

  def errors: Directive[Seq[Message] :: HNil] = messages('error)
  def warnings: Directive[Seq[Message] :: HNil] = messages('warning)
  def notices: Directive[Seq[Message] :: HNil] = messages('notice)

  def messages(level: Symbol): Directive[Seq[Message] :: HNil] = extract { ctx =>
    findMessages(ctx.request, level)
  }

  def messages(): Directive[Map[Symbol, Seq[Message]] :: HNil] = extract { ctx =>
    ctx.request.headers collectFirst {
      case Messages(ms) => ms
    } getOrElse Map.empty
  }
}

object WebDirectives extends WebDirectives