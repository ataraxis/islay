package islay.template.util

import java.nio.file.{Files, Path}

import scala.concurrent.{ExecutionContext, Future}

import spray.caching.LruCache


object ResourceCache {
  case class Entry[A](lastModified: Long, value: A)
}


/**
 * A wrapper around Spray's `LruCache` for resources which checks the last modified time of the
 * resource and re-applies the cached expression on modification.
 */
class ResourceCache[A](reapplyOnModification: Boolean) {

  import ResourceCache._

  val cache = LruCache[Entry[A]]()


  def apply(resource: Path)(expression: => Future[A])
      (implicit executor: ExecutionContext): Future[A] = {

    entry(resource)(expression).map(_.value)
  }

  def entry(resource: Path)(expression: => Future[A])
      (implicit executor: ExecutionContext): Future[Entry[A]] = {

    val entry = cache(resource) {
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
          this.entry(resource)(expression)
        }
        else
          Future.successful(entry)
      }
    }
    else
      entry
  }
}