package islay.template.util

import java.nio.file.{Files, Path}

import scala.concurrent.{ExecutionContext, Future}

import spray.caching.LruCache


/**
 * A wrapper around Spray's `LruCache` for resources which checks the last modified time of the
 * resource and re-applies the cached expression on modification.
 */
class ResourceCache[A](reapplyOnModification: Boolean) {

  val cache = LruCache[Entry[A]]()

  case class Entry[A](lastModified: Long, value: A)

  def fromFuture(resource: Path)(expression: => Future[A])
      (implicit executor: ExecutionContext): Future[A] = {

    val entry = cache.fromFuture(resource) {
      for {
        lastModified <- Future(Files.getLastModifiedTime(resource).toMillis)
        value <- expression
      } yield Entry(lastModified, value)
    }

    if (reapplyOnModification) {
      entry flatMap { entry =>
        val lastModified = Files.getLastModifiedTime(resource).toMillis
        if (lastModified > entry.lastModified) {
          cache.remove(resource)
          fromFuture(resource)(expression)
        }
        else
          Future.successful(entry.value)
      }
    }
    else
      entry.map(_.value)
  }
}