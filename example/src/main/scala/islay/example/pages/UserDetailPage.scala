package islay.example.pages

import islay.example.TemplateProcessorModule
import islay.example.dao.UserDaoModule
import islay.example.model.User
import islay.web.{Form, FormPage, Message, WebDirectives}
import islay.web.components.{SubmitButton, TextInput}
import spray.http.StatusCodes
import spray.routing.Route


trait UserDetailPageModule extends UserDaoModule with TemplateProcessorModule with WebDirectives {

  val notFound: Route = complete(StatusCodes.NotFound)

  def userDetailPage(username: String): Route =
    userDao.findByUsername(username).fold(notFound) { u =>
      new UserDetailPage(u)
    }

  class UserDetailPage(user: User) extends FormPage {

    val form = new Form {
      override val fields = Map(
        "full-name" -> TextInput(user.fullName),
        "ok" -> SubmitButton {
          flash(Message("user.saved", user.username)) {
            refresh("/users") {
              _.complete(StatusCodes.OK)
            }
          }
        }
      )
    }


    def transform = (
      c"title" ** s"Details for ${user.username}" &
      c"form" <> form
    )
  }
}
