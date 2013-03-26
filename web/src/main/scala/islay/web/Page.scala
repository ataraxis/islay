package islay.web

import islay.transform.Transform
import spray.routing.RequestContext
import spray.routing.Route


trait Page extends Route {

  def transform: Transform

  override def apply(context: RequestContext) {
    // apply transformation
    // complete response
  }
}