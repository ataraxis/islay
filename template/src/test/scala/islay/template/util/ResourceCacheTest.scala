package islay.template.util

import java.nio.file.Files
import java.nio.file.attribute.FileTime
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import org.scalatest.{BeforeAndAfterEach, FunSuite, OneInstancePerTest}
import org.scalatest.matchers.ShouldMatchers


class ResourceCacheTest extends FunSuite with ShouldMatchers with BeforeAndAfterEach with OneInstancePerTest {

  import ExecutionContext.Implicits.global

  val resource = Files.createTempFile("resource", null)
  val count = new AtomicInteger(0)

  def operation = Future(count.incrementAndGet)


  override def afterEach() {
    Files.delete(resource)
  }

  test("Expression is applied once for multiple calls against the same path") {
    val cache = new ResourceCache[Int](false)

    val result = Await.result(cache.fromFuture(resource)(operation), 1.second)

    result should be (1)
    count.get should be (1)

    val result2 = Await.result(cache.fromFuture(resource)(operation), 1.second)

    result2 should be (1)
    count.get should be (1)
  }

  test("Expression is reapplied if resource is modified and reapply is enabled") {
    val cache = new ResourceCache[Int](true)

    val result = Await.result(cache.fromFuture(resource)(operation), 1.second)

    result should be (1)
    count.get should be (1)

    Files.setLastModifiedTime(resource, FileTime.fromMillis(System.currentTimeMillis + 5000))

    val result2 = Await.result(cache.fromFuture(resource)(operation), 1.second)

    result2 should be (2)
    count.get should be (2)
  }

  test("Expression is not reapplied if resource is modified and reapply is disabled") {
    val cache = new ResourceCache[Int](false)

    val result = Await.result(cache.fromFuture(resource)(operation), 1.second)

    result should be (1)
    count.get should be (1)

    Files.setLastModifiedTime(resource, FileTime.fromMillis(System.currentTimeMillis + 5000))

    val result2 = Await.result(cache.fromFuture(resource)(operation), 1.second)

    result2 should be (1)
    count.get should be (1)
  }
}