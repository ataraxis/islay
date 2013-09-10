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

  case class WebContext(context: islay.web.WebContext) extends HttpHeader with SingletonValueRenderable {
    def name = "X-Web-Context"
    def lowercaseName = "x-web-context"
  }

}