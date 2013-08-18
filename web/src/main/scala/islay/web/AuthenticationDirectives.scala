package islay.web

import spray.routing.{Directive0, Directive1, RequestContext}
import spray.routing.directives._
import spray.http.HttpHeaders.Cookie
import java.math.BigInteger
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac
import java.security.MessageDigest
import org.parboiled.common.Base64
import spray.http.HttpCookie
import shapeless.HNil
import spray.routing.AuthenticationFailedRejection

trait AuthenticationDirectives {

  import BasicDirectives._
  import CookieDirectives._
  import RouteDirectives._

  def authenticated: Directive1[String] = userToken flatMap {
    case Some(token) => provide(token)
    case None => reject(AuthenticationFailedRejection("islay"))
  }

  private case class AuthSession(creationTimeMs: Long, touchTimeMs: Long, userToken: String) {
    private val hmac: Array[Byte] = computeMac(creationTimeMs, userToken.getBytes("UTF-8"))
    val authenticator: String = Base64.custom.encodeToString(hmac, false)

    def isValid(token: String, timestampMs: Long): Boolean = (
      MessageDigest.isEqual(Base64.custom.decodeFast(token), hmac) &&
        creationTimeMs + WebSettings.absoluteTimeoutMs > timestampMs &&
        touchTimeMs + WebSettings.idleTimeoutMs > timestampMs
    )
  }

  def userToken: Directive1[Option[String]] = extract { ctx =>

    def get(name: String) = ctx.request.headers.collectFirst {
      case Cookie(cookies) =>
        cookies.find(_.name == name).map(_.content)
    }.flatten

    (get("#"), get("^"), get("$"), get("!")) match {
      case (Some(authenticator), Some(creationTime), Some(touchTime), Some(token)) =>
        try {
          val creationTimeMs = creationTime.toLong
          val touchTimeMs = touchTime.toLong
          val authSession = AuthSession(creationTimeMs, touchTimeMs, token)
          if (authSession.isValid(authenticator, System.currentTimeMillis))
            Some(token)
          else
            None
        }
        catch { case _: Exception =>
          None
        }
      case _ =>
        None
    }
  }

  private def computeMac(creationTimeMs: Long, data: Array[Byte]): Array[Byte] = {
    val key = WebSettings.secretKey
    val keySpec = new SecretKeySpec(key, WebSettings.hmacAlgorithm)
    val mac = Mac.getInstance(WebSettings.hmacAlgorithm)
    mac.init(keySpec)
    mac.doFinal(data)
  }

  def isSecure: Directive1[Boolean] = extract(_.request.uri.scheme == "https")

  def authenticate(userToken: String): Directive0 = isSecure flatMap { secure =>
    val now = System.currentTimeMillis
    val session = AuthSession(now, now, userToken)
    def cookie(name: String, content: String) = HttpCookie(name, content, secure = secure, httpOnly = true)
    setCookie(
      cookie("^", now.toString),
      cookie("$", now.toString),
      cookie("!", userToken),
      cookie("#", session.authenticator)
    )
  }

  def touchAuthSession: Directive0 = {
    ???
  }

  def unauthenticate: Directive0 = {
    def cookie(name: String) = HttpCookie(name, "", httpOnly = true)
    deleteCookie(cookie("^"), cookie("$"), cookie("!"), cookie("#"))
  }
}