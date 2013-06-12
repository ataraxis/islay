package islay.example

import islay.example.pages.UsersPageModule
import islay.web.Directives
import spray.routing.Route
import spray.routing.directives.PathMatchers
import islay.example.pages.UserDetailPageModule


trait Routes
  extends UsersPageModule
  with UserDetailPageModule
  with TemplateProcessorModule {

  import Directives._
  import PathMatchers._


  def route: Route = (
    path("users") {
      dynamic(usersPage)
    } ~
    path("user" / PathElement) { username =>
      rewritePath("user") {
        userDetailPage(username)
      }
    }
  )
}