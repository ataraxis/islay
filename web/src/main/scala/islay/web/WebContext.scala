package islay.web

import java.util.Locale
import spray.routing.RequestContext
import spray.http.HttpRequest
import scala.concurrent.ExecutionContext
import islay.template.ParentRequest


object WebContext {

  def from(request: HttpRequest): WebContext = {
    def context(req: HttpRequest) = req.headers.collectFirst { case WebHeaders.WebContext(c) => c }
    def parent(req: HttpRequest) = req.headers.collectFirst { case ParentRequest(r) => r }
    def go(req: HttpRequest): WebContext = context(req) getOrElse parent(req).fold(WebContext(req))(go)
    go(request)
  }

  def apply(request: HttpRequest): WebContext = WebContext(request, Locales.from(request))
}

case class WebContext(
    request: HttpRequest,
    locales: Seq[Locale],
    messages: Map[Symbol, Seq[Message]] = Map.empty,
    attributes: Map[String, Any] = Map.empty
) {

  def errors: Seq[Message] = messages.getOrElse('error, Nil)
  def alerts: Seq[Message] = messages.getOrElse('alert, Nil)
  def notices: Seq[Message] = messages.getOrElse('notice, Nil)

  def error(name: String): Seq[Message] = errors collect {
    case m: FieldError if m.fieldName == name => m
  }

  /**
   * All request-scope and flash-scope messages in a single collection.
   */
  def allMessages(implicit executor: ExecutionContext): Map[Symbol, Seq[Message]] =
    messages ++ flashMessages

  /**
   * Add request-scope message to this request.
   */
  def addMessages(level: Symbol, msgs: Seq[Message]): WebContext = {
    val seq = messages.get(level) match {
      case Some(seq) => msgs ++: seq
      case None => msgs
    }
    copy(messages = messages + ((level, seq)))
  }

  /**
   * Flash-scope alerts passed to this request.
   */
  def flashErrors(implicit executor: ExecutionContext): Seq[Message] =
    flashMessages.getOrElse('error, Nil)

  /**
   * Flash-scope alerts passed to this request.
   */
  def flashAlerts(implicit executor: ExecutionContext): Seq[Message] =
    flashMessages.getOrElse('alert, Nil)

  /**
   * Flash-scope notices passed to this request.
   */
  def flashNotices(implicit executor: ExecutionContext): Seq[Message] =
    flashMessages.getOrElse('notice, Nil)

  /**
   * All flash-scope messages passed to this request.
   */
  def flashMessages(implicit executor: ExecutionContext): Map[Symbol, Seq[Message]] = {
    implicit val ls = locales
    request.uri.query.getAll("!flash") map { param =>
      Message.deserialize(param)
    } groupBy (_._1) mapValues (_.map(_._2))
  }


  /**
   * Returns a new `HttpRequest` which stores this context.
   */
  def withinRequest: HttpRequest = {
    var found = false
    val hs = request.headers map {
      case _: WebHeaders.WebContext =>
        found = true
        WebHeaders.WebContext(this)
      case h => h
    }
    val headers = if (found) hs else WebHeaders.WebContext(this) :: hs
    request.copy(headers = headers)
  }
}