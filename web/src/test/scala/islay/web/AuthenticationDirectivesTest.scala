package islay.web

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import spray.http.{HttpHeaders, StatusCodes}
import spray.routing.directives.{CookieDirectives, RouteDirectives}
import spray.testkit.ScalatestRouteTest


class AuthenticationDirectivesTest extends FunSuite with ScalatestRouteTest with ShouldMatchers
with AuthenticationDirectives with RouteDirectives with CookieDirectives {

  test("The `authenticate` directive adds valid authenticator to response") {
    Get() ~> authenticate("bertrand") { complete(StatusCodes.OK) } ~> check {
      val cookies = response.headers.collect {
        case HttpHeaders.`Set-Cookie`(c) => (c.name, c.content)
      }.toMap
      val authenticator = cookies("#")
      val creationTime = cookies("^").toLong
      val updateTime = cookies("$").toLong
      val userToken = cookies("!")

      userToken should be ("bertrand")
      creationTime should equal (updateTime)

      val session = AuthSession(creationTime, updateTime, userToken)
      session.authenticator should equal (authenticator)
    }
  }

  test("An authentication session's own authenticator is valid") {
    val now = System.currentTimeMillis
    val session = AuthSession(now, now, "harvey")
    val authenticator = session.authenticator

    session.isValid(authenticator) should equal (true)
  }

  test("An authentication session is not expired if still within the idle timeout") {
    val now = System.currentTimeMillis
    val fiveHoursFromNow = now + 5*60*60*1000L
    val session = AuthSession(now, now, "edwin")

    val twentyFiveMinutesLater = now + 25*60*1000L
    session.isExpired(twentyFiveMinutesLater) should equal (false)
  }

  test("An authentication session is expired if past the idle timeout") {
    val now = System.currentTimeMillis
    val tenHoursFromNow = now + 10*60*60*1000L
    val session = AuthSession(now, tenHoursFromNow, "humphrey")

    val thirtyFiveMinutesLater = tenHoursFromNow + 35*60*1000L
    session.isExpired(thirtyFiveMinutesLater) should equal (true)
  }

  test("An authentication session is expired if past the absolute timeout") {
    val now = System.currentTimeMillis
    val twentyFourHoursFromNow = now + 24*60*60*1000L
    val almostTwentyFourHoursFromNow = twentyFourHoursFromNow - 60000
    val session = AuthSession(now, almostTwentyFourHoursFromNow, "clifford")

    val fiveMinutesLater = almostTwentyFourHoursFromNow + 5*60*1000L
    session.isExpired(fiveMinutesLater) should equal (true)
  }
}