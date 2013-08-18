package islay.example

import islay.example.pages.{LoginPageModule, UserDetailPageModule, UsersPageModule}
import islay.web.Directives
import spray.http.StatusCodes
import spray.http.Uri.apply
import spray.routing.Route


trait Routes
  extends LoginPageModule
  with UsersPageModule
  with UserDetailPageModule
  with TemplateProcessorModule {

  import Directives._


  def route: Route = (
    path("login")(loginPage) ~
    authenticated { loggedInUser =>
      path("users")(usersPage) ~
      path("user" / Segment) { username =>
        rewritePath("/user") {
          userDetailPage(username)
        }
      } ~
      path("logout") {
        unauthenticate {
          redirect("/login", StatusCodes.Found)
        }
      }
    } ~
    defaultRoute
  )
}