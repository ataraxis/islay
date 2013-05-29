package islay.web

import java.nio.file.{Files, NoSuchFileException, Path}
import java.nio.file.attribute.BasicFileAttributes
import java.util.Locale

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import islay.template.util.Resources


class MessageResourceLoaderTest extends FunSuite with ShouldMatchers {

  import ExecutionContext.Implicits.global
  val resourceLoader = new MessageResourceLoader(Resources.pathTo("properties"), false)

  def lastAccessTime(path: Path) = {
    val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
    attrs.lastAccessTime.toMillis
  }

  /* XXX: not an accurate test if partition is mounted with relatime (the Linux default) */
  test("Resources are not loaded for locales that follow a match") {
    val messagesEnPath = Resources.pathTo("properties").resolve("messages_en.properties")
    val startingAccessTime = lastAccessTime(messagesEnPath)

    val locales = Seq(new Locale("en", "US"), new Locale("en"))
    val f = resourceLoader.lookup("messages", locales)

    val props = Await.result(f, 2.seconds)
    props.locale should equal (new Locale("en", "US"))
    props.get("greeting") should equal (Some("Dude"))

    lastAccessTime(messagesEnPath) should equal (startingAccessTime)
  }

  test("The default resource is returned if there are no matches for the provided locales") {
    val locales = Seq(new Locale("fr"))
    val f = resourceLoader.lookup("messages", locales)

    val props = Await.result(f, 2.seconds)
    props.locale should equal (Locale.getDefault)
    props.get("greeting") should equal (Some("Ahoy"))
  }

  test("A Failure with NoSuchFileException is returned for a failed lookup") {
    val locales = Seq(new Locale("fr"))
    val f = resourceLoader.lookup("quotes", locales)

    intercept[NoSuchFileException] {
      Await.result(f, 2.seconds)
    }
  }
}