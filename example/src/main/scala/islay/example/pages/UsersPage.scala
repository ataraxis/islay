package islay.example.pages

import islay.example.TemplateProcessorModule
import islay.example.dao.UserDaoModule
import islay.transform._
import islay.web.Page


trait UsersPageModule extends UserDaoModule with TemplateProcessorModule {


  def usersPage: UsersPage = new UsersPage

  class UsersPage extends Page {

    def transform = (
      c".users" <> userDao.findAll.map ( user =>
        c".username" ** user.username &
        c".full-name" ** user.fullName
      )
    )
  }
}