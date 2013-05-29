package islay.web

import java.io.ByteArrayInputStream
import java.nio.file.{NoSuchFileException, Path}
import java.util.{Locale, Properties}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Success

import islay.template.util.{ResourceCache, Resources}


object MessageResourceLoader extends MessageResourceLoader(Resources.pathTo("properties"), WebSettings.reloadResources)


class MessageResourceLoader(basePath: Path, reloadResources: Boolean) {

  private val cache = new ResourceCache[MessageProps](reloadResources)


  /**
   * Returns message properties for the first locale in `locales` that corresponds to a resource of
   * the form `<name>_<locale>.properties`. The `<locale>` part of the resource name should have
   * tags separated by underscore, for example `en_US`.
   *
   * If no resource can be found for any locale, then a final attempt will be made load a resource
   * at `<name>.properties` associated with the default locale. Otherwise a `NoSuchFileException`
   * Failure will be returned.
   */
  def lookup(name: String, locales: Seq[Locale])
      (implicit executor: ExecutionContext): Future[MessageProps] = {

    val notFound = Future.failed[MessageProps](new NoSuchFileException(name))

    paths(name, locales).foldLeft(notFound) { case (acc, (path, locale)) =>
      orElse(acc, cache.fromFuture(path)(loadResource(path, locale)))
    }
  }

  /* like fallbackTo, but with by-name second argument to make sequential */
  private def orElse(first: Future[MessageProps], other: => Future[MessageProps])
      (implicit executor: ExecutionContext): Future[MessageProps] = {
    val p = Promise[MessageProps]()
    first.onComplete {
      case s @ Success(_) => p complete s
      case _ => p completeWith other
    }
    p.future
  }

  private def paths(name: String, locales: Seq[Locale]): Seq[(Path, Locale)] = {
    val paths =
      for (locale <- locales)
      yield (basePath.resolve(name +"_"+ locale +".properties"), locale)

    paths :+ (basePath.resolve(name +".properties"), Locale.getDefault)
  }

  private def loadResource(resource: Path, locale: Locale)
      (implicit executor: ExecutionContext): Future[MessageProps] = {

    Resources.readAllBytes(resource) map { bytes =>
      val in = new ByteArrayInputStream(bytes)
      val props = new Properties
      props.load(in)
      val map = collection.JavaConversions.propertiesAsScalaMap(props).toMap
      MessageProps(map, locale)
    }
  }
}


/**
 * A map of strings loaded from a properties file together with the locale that was used to locate
 * the file.
 */
case class MessageProps(props: Map[String, String], locale: Locale) {
  def get(key: String): Option[String] = props.get(key)
}