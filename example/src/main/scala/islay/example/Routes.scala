package islay.example

import spray.routing.Route
import spray.routing.directives.PathMatchers
import islay.web.Directives
import islay.example.pages.UsersPageModule


trait Routes
  extends UsersPageModule
  with TemplateProcessorModule {

  import Directives._
  import PathMatchers._


  def route: Route = (
    path("users") {
      usersPage
    }
  )
}