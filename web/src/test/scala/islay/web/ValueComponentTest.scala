package islay.web

import java.util.Locale

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import islay.web.components.TextInput


class ValueComponentTest extends FunSuite with ShouldMatchers {

  import ExecutionContext.Implicits.global
  implicit val locales = Seq(Locale.ENGLISH)

  test("Field conversion success results in a value") {

    val input = TextInput(3.14)()
    input.complete("price", Some(TextValue("8.90")))

    input.value should be (8.90)
    val messages = Await.result(input.errors, 2.seconds)
    messages.length should be (0)
  }

  test("Field conversion failure results in an error") {

    val input = TextInput(3.14)(validator = { _ => ??? })
    input.complete("price", Some(TextValue("tree fiddy")))

    val messages = Await.result(input.errors, 2.seconds)
    messages.length should be (1)
    messages(0).toString should be ("Invalid decimal number")
    input.value should be (3.14)
    input.valueAsString should be ("tree fiddy")
  }

  test("Multiple validation errors appear in the same order as created") {

    def validateQuantity(v: Int): Error = {
      Error(<span>Must be at least 10.</span>).when(v < 10) +
      Error(<span>Must be odd.</span>).when(v % 2 == 0)
    }

    val input = TextInput(11)(validator = validateQuantity)
    input.complete("quantity", Some(TextValue("8")))

    val messages = Await.result(input.errors, 2.seconds)
    messages.length should be (2)
    messages(0).toString should be ("<span>Must be at least 10.</span>")
    messages(1).toString should be ("<span>Must be odd.</span>")
    input.value should be (8)
  }

  test("Field name and value are first arguments to message") {

    def validateFirstName(name: Option[String]): Error = {
      Error("first-name.palindrome").when (name.exists(n => n == n.reverse)) +
      Error("first-name.illegal-character", "/").when (name.exists(_.contains("/")))
    }

    val input = TextInput(Option("Harold"))(validator = validateFirstName)
    input.complete("first-name", Some(TextValue("p/p")))

    val messages = Await.result(input.errors, 2.seconds)
    messages.length should be (2)
    Await.result(messages(0).toNodeSeq, 2.seconds).toString should be ("<cite>First name</cite> cannot be a palindrome.")
    Await.result(messages(1).toNodeSeq, 2.seconds).toString should be ("<em>p/p</em> contains illegal character '/'.")
  }

  test("Required field produces validation error if empty") {

    val input = TextInput("")(required = true)
    input.complete("first-name", Some(TextValue("")))

    val messages = Await.result(input.errors, 2.seconds)
    messages.length should be (1)
    Await.result(messages(0).toNodeSeq, 2.seconds).toString should be ("<cite>First name</cite> is required.")
  }
}