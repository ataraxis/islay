package islay.example

import islay.example.pages.{UserDetailPageModule, UsersPageModule}
import spray.routing._


trait Routes
  extends UsersPageModule
  with UserDetailPageModule
  with TemplateProcessorModule {

  import Directives._


  def route: Route = (
    path("users") {
      dynamic(usersPage)
    } ~
    path("user" / Segment) { username =>
      rewritePath("user") {
        userDetailPage(username)
      }
    }
  )
}