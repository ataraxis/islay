package islay.web

import WebHeaders.RequestAttributes
import spray.http.HttpHeader
import spray.http.HttpRequest
import spray.routing.RequestContext


class WebContext(private val requestContext: RequestContext) {

  val request: HttpRequest = requestContext.request
  val attributes: Map[String, Any] = findAttributes(request.headers)

  private def findAttributes(headers: List[HttpHeader]) =
    headers.foldLeft(Map.empty[String, Any]) {
      case (_, h: RequestAttributes) => h.attributes
      case (acc, _) => acc
    }
}