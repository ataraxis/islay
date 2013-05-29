package islay.example

import islay.example.pages.UsersPageModule
import islay.web.Directives
import spray.routing.Route
import spray.routing.directives.PathMatchers


trait Routes
  extends UsersPageModule
  with TemplateProcessorModule {

  import Directives._
  import PathMatchers._


  def route: Route = (
    path("users") {
      dynamic {
        usersPage
      }
    }
  )
}