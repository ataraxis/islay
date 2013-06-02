package islay.template

import com.typesafe.config.ConfigFactory


private[islay] object TemplateSettings {

  private val config = ConfigFactory.load().getConfig("islay.template")

  def reloadResources: Boolean = config.getBoolean("reload-resources")
}