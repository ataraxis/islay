package islay.web

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import islay.transform.CallingThreadExecutor
import spray.http.{HttpHeader, HttpHeaders, HttpRequest, HttpResponse, Rendering, SingletonValueRenderable, Uri}


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

  def addFlashMessage(response: HttpResponse, level: Symbol, content: Message): Future[HttpResponse] = {
    import CallingThreadExecutor.Implicit
    val newHeaders = response.headers map {
      case Refresh(timeout, url) =>
        flashUrl(url, level, content) map { url => Refresh(timeout, url) }
      case HttpHeaders.Location(url) =>
        flashUrl(url, level, content) map { url => HttpHeaders.Location(url) }
      case other =>
        Future.successful(other)
    }
    Future.sequence(newHeaders).map(response.withHeaders)
  }

  private def flashUrl(url: Uri, level: Symbol, content: Message) = {
    import CallingThreadExecutor.Implicit
    content.serialize(level) map { content =>
      url.copy(query = Uri.Query.Cons("!flash", content, url.query))
    }
  }


  def messages(request: HttpRequest): Map[Symbol, Seq[Message]] =
    requestMessages(request) ++ flashMessages(request)

  def requestMessages(request: HttpRequest): Map[Symbol, Seq[Message]] = {
    request.headers collectFirst {
      case Messages(ms) => ms
    } getOrElse Map.empty
  }

  def flashMessages(request: HttpRequest): Map[Symbol, Seq[Message]] = {
    /* XXX: taking ExecutionContext as implicit param causes weirdness with directives */
    import ExecutionContext.Implicits.global
    implicit val locales = Locales.from(request)
    request.uri.query.getAll("!flash") map { param =>
      Message.deserialize(param)
    } groupBy (_._1) mapValues (_.map(_._2))
  }
}