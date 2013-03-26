package islay.web

import com.typesafe.config.ConfigFactory

object WebSettings {

  private val config = ConfigFactory.load().getConfig("islay.web")

  def reloadResources: Boolean = config.getBoolean("reload-resources")
}