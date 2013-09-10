package islay.web

import java.nio.file.{Files, Paths}
import java.security.SecureRandom

import com.typesafe.config.ConfigFactory

object WebSettings {

  private val config = ConfigFactory.load().getConfig("islay.web")

  val reloadResources: Boolean = config.getBoolean("reload-resources")

  val secretPath: String = config.getString("authenticator.secret-path")
  val hmacAlgorithm: String = config.getString("authenticator.hmac-algorithm")
  val idleTimeoutMs: Long = config.getMilliseconds("authenticator.idle-timeout")
  val absoluteTimeoutMs: Long = config.getMilliseconds("authenticator.absolute-timeout")

  val secretKey: Array[Byte] = secretPath match {
    case null | "" =>
      val a = new Array[Byte](20)
      (new SecureRandom).nextBytes(a)
      a
    case _ =>
      Files.readAllBytes(Paths.get(secretPath))
  }
}