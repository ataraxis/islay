package islay.template

import java.nio.file.Files

import scala.xml.{Comment, Elem}

import org.scalatest.{Finders, FunSuite}
import org.scalatest.matchers.ShouldMatchers


class Html5ParserTest extends FunSuite with ShouldMatchers {

  val root = Resources.pathTo("webapp")
  val htmlFile = Resources.resolve(root, "buffalo.html")


  test("Comments are preserved in order in parsed HTML") {
    val parser = new Html5Parser
    val ns = parser.parse(Files readAllBytes htmlFile)

    val children = (ns \\ "div").collect{ case div: Elem => div.child }.flatten

    children should have length (3)
    children(0).text.trim should be ("Buffalo")
    children(1).asInstanceOf[Comment].commentText.trim should be ("buffalo?")
    children(2).text.trim should be ("Buffalo!")
  }
}