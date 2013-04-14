package islay.template

import java.nio.file.{Files, InvalidPathException}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.DurationInt

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class ResourcesTest extends FunSuite with ShouldMatchers {

  val root = Resources.pathTo("webapp")

  test("A resource can be resolved containing `..` if it stays under the root") {
    val path = Resources.resolve(root, "foo/../index.html")
    Files.exists(path) should be (true)
  }

  test("A resource cannot be resolved that traverses the root path") {
    intercept[InvalidPathException] {
      Resources.resolve(root, "../webapp/index.html")
    }
  }

  test("A path starting with `/` is resolved relative to the root") {
    val path = Resources.resolve(root, "///index.html")
    Files.exists(path) should be (true)
  }

  test("`readAllBytes` can read a file from the classpath") {
    import ExecutionContext.Implicits.global

    val path = Resources.resolve(root, "index.html")
    val f = Resources.readAllBytes(path)

    val result = Await.result(f, 2.seconds)
    new String(result) should equal ("<html/>")
  }
}