package islay.example.pages

import islay.example.TemplateProcessorModule
import islay.example.dao.UserDaoModule
import islay.transform._
import islay.web.{AuthenticationDirectives, Error, Form, FormPage}
import islay.web.components.{SubmitButton, TextInput}
import spray.http.StatusCodes
import spray.routing.Route


trait LoginPageModule extends UserDaoModule with TemplateProcessorModule with AuthenticationDirectives {


  def loginPage: Route = dynamic(new LoginPage)

  class LoginPage extends FormPage {

    val form = new Form {
      val username = TextInput("")(required = true)
      val password = TextInput("")(required = true)
      override val fields = Map(
        "username" -> username,
        "password" -> password,
        "login" -> SubmitButton {
          authenticate(username.value) {
            redirect("/users", StatusCodes.Found)
          }
        }
      )

      override def validate: Error = (
        Error.text("Invalid username or password")
          .when (isWithoutFieldErrors)
          .and (username.value.length < 4 || password.value.length < 3)
      )

    }

    def transform = (
      c"form" <> form
    )
  }
}