package islay.web

import spray.routing._
import spray.routing.directives._
import spray.http.HttpHeader

import shapeless._


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
  import RespondWithDirectives._
  import MiscDirectives._
  import WebHeaders._

  def refresh(url: String): Directive0 = refresh(0, url)

  def refresh(timeout: Int, url: String): Directive0 = respondWithHeader(Refresh(timeout, url))

  def replacePath(path: String): Directive0 = rewriteUnmatchedPath(_ => path)

  def flash(content: String): Directive0

  def requestAttr[T](name: String): Directive[Option[T] :: HNil] = null
  def requestAttr(name: String, value: Any): Directive0 = null
}