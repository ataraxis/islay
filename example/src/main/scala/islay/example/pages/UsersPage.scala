package islay.example.pages

import islay.example.TemplateProcessorModule
import islay.example.dao.UserDaoModule
import islay.transform._
import islay.web.{Message, Page}
import spray.routing.Route


trait UsersPageModule extends UserDaoModule with TemplateProcessorModule {


  def usersPage: Route = dynamic(new UsersPage)

  class UsersPage extends Page {

    def transform = (
      c"table".before(Message("greeting")) &
      c".users" <> userDao.findAll.map ( user =>
        c".username" ** user.username &
        c".full-name" ** user.fullName &
        c"a.username".addAttr("href", "/user/"+ user.username)
      )
    )
  }
}