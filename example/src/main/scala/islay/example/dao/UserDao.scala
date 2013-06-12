package islay.example.dao

import islay.example.model.User


trait UserDaoModule {

  val userDao = new UserDao

  class UserDao {

    private val users = Set(
      User("lulu", "proust", "Louise Brooks"),
      User("greta", "ninotchka", "Greta Garbo"),
      User("clara", "it", "Clara Bow")
    )

    def findAll: Seq[User] = users.toSeq

    def findByUsername(username: String): Option[User] =
      findAll.filter(_.username == username).headOption
  }
}