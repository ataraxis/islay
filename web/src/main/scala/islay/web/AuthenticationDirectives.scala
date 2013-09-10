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
import spray.http.HttpRequest


object AuthSession {

  /**
   * Extracts an authentication session from a request if it can be verified that the user has
   * authenticated within a configured time period and the session has not timed out.
   */
  def from(request: HttpRequest): Option[AuthSession] = {

    def get(name: String) = request.headers.collectFirst {
      case Cookie(cookies) =>
        cookies.find(_.name == name).map(_.content)
    }.flatten

    (get("#"), get("^"), get("$"), get("!")) match {
      case (Some(authToken), Some(creationTime), Some(touchTime), Some(userToken)) =>
        try {
          val creationTimeMs = creationTime.toLong
          val touchTimeMs = touchTime.toLong
          val session = AuthSession(creationTimeMs, touchTimeMs, userToken)
          if (session.isValid(authToken) && !session.isExpired(System.currentTimeMillis))
            Some(session)
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
}

/**
 * A client-cacheable authentication session which stores:
 *
 * - Timestamp when the user was last authenticated (creation time)
 * - Timestamp of the last HTTP request (touch time)
 * - A value identifying the user that was authenticated (user token)
 *
 * The creation time and user token are protected against client tampering through a message
 * authentication code, the touch time is not. In combination with SSL, this provides a secure,
 * easy-to-use mechanism that doesn't rely on server-side state.
 *
 * This implementation follows the scheme described by Fu et al in "Dos and Don'ts of Client
 * Authentication on the Web" - http://cookies.lcs.mit.edu/pubs.html
 */
case class AuthSession(creationTimeMs: Long, touchTimeMs: Long, userToken: String) {

  private val hmac: Array[Byte] = computeMac(creationTimeMs, userToken.getBytes("UTF-8"))

  /**
   * A cryptographically secure token tied to the creation time and user token of this session.
   */
  val authenticator: String = Base64.custom.encodeToString(hmac, false)

  /**
   * Checks if an authentication token matches the authenticator derived from the attributes of
   * this session.
   */
  def isValid(token: String): Boolean =
    MessageDigest.isEqual(Base64.custom.decodeFast(token), hmac)

  /**
   * Checks if this session has expired (either due to an idle timeout or absolute timeout) with
   * respect to the timestamp provided.
   */
  def isExpired(currentTimeMs: Long) = (
    creationTimeMs + WebSettings.absoluteTimeoutMs < currentTimeMs ||
    touchTimeMs + WebSettings.idleTimeoutMs < currentTimeMs
  )

  private def computeMac(creationTimeMs: Long, data: Array[Byte]): Array[Byte] = {
    val key = WebSettings.secretKey
    val keySpec = new SecretKeySpec(key, WebSettings.hmacAlgorithm)
    val mac = Mac.getInstance(WebSettings.hmacAlgorithm)
    mac.init(keySpec)
    mac.doFinal(data)
  }
}


trait AuthenticationDirectives {

  import BasicDirectives._
  import CookieDirectives._
  import RouteDirectives._

  /**
   * Checks for the presence of a valid authentication session and extracts the associated user
   * token. A session is considered valid if the user has authenticated within the amount of time
   * specified by the setting `islay.web.authenticator.absolute-timeout` and the session has not
   * been idle for longer than `islay.web.authenticator.idle-timeout`.
   */
  def authenticated: Directive1[String] = userToken flatMap {
    case Some(token) => provide(token)
    case None => reject(AuthenticationFailedRejection("islay"))
  }

  def userToken: Directive1[Option[String]] = extract { ctx =>
    AuthSession.from(ctx.request).map(_.userToken)
  }

  /**
   * Whether this connection is secured using SSL/TLS.
   */
  def isSecure: Directive1[Boolean] = extract(_.request.uri.scheme == "https")

  /**
   * Creates an authentication session for the user identified by `userToken` and adds http-only
   * cookies to the response containing its session information. Subsequent requests can check
   * for the presence of a valid authentication token using the `authenticated` directive.
   *
   * A secret key is required by the server to calculate authentication tokens and verify that
   * session information has not been tampered with. The path to this key is specified by the
   * setting `web.islay.authenticator.secret-path`, which if empty will cause a new, random key
   * to be generated every time the server restarts (making existing sessions invalid).
   *
   * This directive applies to both SSL and plain-text connections, however sessions authenticated
   * over SSL will not apply to subsequent requests unless they are also SSL-encrypted.
   *
   * Note that this mechanism is only for authenticating the client. It does not provide any kind
   * of confidentiality. That's what SSL is for.
   */
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

  /**
   * Deletes the cookies containing authentication session information.
   */
  def unauthenticate: Directive0 = {
    def cookie(name: String) = HttpCookie(name, "", httpOnly = true)
    deleteCookie(cookie("^"), cookie("$"), cookie("!"), cookie("#"))
  }
}