package islay.template

import scala.xml.{Comment, Elem}

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class Html5ParserTest extends FunSuite with ShouldMatchers {

  val parser = new Html5Parser

  test("Comments are preserved in order in parsed HTML") {
    val bytes = """<div>
      |Buffalo
      |<!-- buffalo? -->
      |Buffalo!
      |</div>""".stripMargin.getBytes

    val ns = parser.parse(bytes)

    val children = (ns \\ "div").collect{ case div: Elem => div.child }.flatten

    children should have length (3)
    children(0).text.trim should be ("Buffalo")
    children(1).asInstanceOf[Comment].commentText.trim should be ("buffalo?")
    children(2).text.trim should be ("Buffalo!")
  }

  test("Comments are preserved at start of file") {
    val bytes = "<!-- there is no there there --><br/>".getBytes
    val ns = parser.parse(bytes)

    ns.toString should equal ("<!-- there is no there there --><br/>")
  }

  test("Body attributes are preserved") {
    val bytes = """<body id="bod"><br></body>""".getBytes
    val ns = parser.parse(bytes)

    ns.toString should equal ("""<body id="bod"><br/></body>""")
  }

  test("Head without body is preserved") {
    val bytes = "<head><title>Foo</title></head>Hello".getBytes
    val ns = parser.parse(bytes)

    ns.toString should equal ("<head><title>Foo</title></head>Hello")
  }
}