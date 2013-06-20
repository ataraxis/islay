package islay.template

import scala.collection.JavaConversions

import com.typesafe.config.ConfigFactory


private[islay] object TemplateSettings {

  private val config = ConfigFactory.load().getConfig("islay.template")

  val reloadResources: Boolean = config.getBoolean("reload-resources")

  val staticPaths: Iterable[String] = JavaConversions.collectionAsScalaIterable(config.getStringList("static-paths"))
}