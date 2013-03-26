package islay.transform.parser

import org.parboiled.errors.{ParserRuntimeException, ParsingException}
import org.parboiled.scala.EOI
import org.parboiled.scala.parserunners.{ReportingParseRunner, TracingParseRunner}
import org.parboiled.scala.rules.Rule1
import org.scalatest.{Assertions, FunSuite}
import org.scalatest.matchers.ShouldMatchers

import islay.transform.Selector


class SelectorParserTest extends FunSuite with ShouldMatchers with Assertions {


  val parser = new SelectorParser() {
    override val buildParseTree = true
  }

  def parse[T](rule: Rule1[T], expression: String): T = {
    val res = ReportingParseRunner(rule).run(expression)
    ReportingParseRunner(rule ~ EOI).run(expression).result match {
      case Some(r) => r
      case None => throw new ParsingException
    }
  }

  def trace[T](rule: Rule1[T], expression: String): T = {
    TracingParseRunner(rule ~ EOI).run(expression).result match {
      case Some(r) => r
      case None => throw new ParsingException
    }
  }

  import parser._


  test("Escape followed by six hex chars is decoded") {
    parse(escapeSequence, "\\000041") should equal ("A")
  }

  test("Escape followed by hex chars > 0x00fffd fails to parse") {
    intercept[ParserRuntimeException] { parse(escapeSequence, "\\010000") }
  }

  test("Escape followed by < 6 hex chars and whitespace is decoded") {
    parse(escapeSequence, "\\41 ") should equal ("A")
  }

  test("Escape followed by < 6 hex chars without whitespace is invalid") {
    intercept[ParsingException] { parse(escapeSequence, "\\a") }
  }

  test("Escape cancels meaning of CSS special chars") {
    parse(escapeSequence, "\\.") should equal (".")
  }

  test("Escape at EOI is literal backslash") {
    parse(escapeSequence, "\\") should equal ("\\")
  }

  test("Identifier containing alphanumeric characters is valid") {
    parse(identifier, "abc_123") should equal ("abc_123")
  }

  test("Identifier starting with number is invalid") {
    intercept[ParsingException] { parse(identifier, "123abc") }
  }

  test("Identifier starting with single dash is valid") {
    parse(identifier, "-foo") should equal ("-foo")
  }

  test("Identifier starting with double dash is invalid") {
    intercept[ParsingException] { parse(identifier, "--foo") }
  }

  test("Identifier containing CSS special characters is invalid") {
    intercept[ParsingException] { parse(identifier, ".foo") }
  }

  test("Identifier containing escaped CSS special characters is valid") {
    parse(identifier, "\\.foo") should equal (".foo")
  }

  test("Identifier containing escaped hex chars is valid") {
    parse(identifier, "\\66 oo") should equal ("foo")
  }

  test("ID selector produces corresponding node") {
    parse(idSelector, "#foo") should equal (IdSelector("foo"))
  }

  test("Class selector produces corresponding node") {
    parse(classSelector, ".foo") should equal (ClassSelector("foo"))
  }

  test("Double quoted CSS string is unescaped") {
    parse(cssString, """"Foo !\= \"Foo\""""") should equal ("""Foo != "Foo"""")
  }

  test("Single quoted CSS string is unescaped") {
    parse(cssString, """'Foo \!= \'Foo\''""") should equal ("Foo != 'Foo'")
  }

  test("Improperly quoted CSS string is invalid") {
    intercept[ParsingException] { parse(cssString, """'Foo != \'Foo\'"""") }
  }

  test("Attribute presence selector produces corresponding node") {
    parse(attributeSelector, "[foo]") should equal (AttributeSelector("foo", None))
  }

  import AttributeOperator._

  test("Attribute exact value selector produces corresponding node") {
    parse(attributeSelector, "[foo=bar]") should equal (AttributeSelector("foo", Some((ExactValue, "bar"))))
  }

  test("Attribute space separated selector produces corresponding node") {
    parse(attributeSelector, "[foo~=bar]") should equal (AttributeSelector("foo", Some((SpaceSeparated, "bar"))))
  }

  test("Attribute dash started selector produces corresponding node") {
    parse(attributeSelector, "[foo|=bar]") should equal (AttributeSelector("foo", Some((FirstDash, "bar"))))
  }

  test("Attribute beginning selector produces corresponding node") {
    parse(attributeSelector, "[foo^=bar]") should equal (AttributeSelector("foo", Some((Beginning, "bar"))))
  }

  test("Attribute ending selector produces corresponding node") {
    parse(attributeSelector, "[foo$=bar]") should equal (AttributeSelector("foo", Some((Ending, "bar"))))
  }

  test("Attribute substring selector produces corresponding node") {
    parse(attributeSelector, "[foo*=bar]") should equal (AttributeSelector("foo", Some((Substring, "bar"))))
  }

  test("Attribute selector with unrecognized operator fails to parse") {
    intercept[ParserRuntimeException] { parse(attributeSelector, "[foo!=bar]") }
  }

  test("Attribute selector with CSS string is valid") {
    parse(attributeSelector, "[foo *= 'bar']") should equal (AttributeSelector("foo", Some((Substring, "bar"))))
  }

  test("Type selector can begin sequence of simple selectors") {
    val SingleSelector(seq, chain) = parse(singleSelector, "p#foo.bar[href^=http]")
    seq should have length (4)
    seq(0) should equal (TypeSelector("p"))
    seq(1) should equal (IdSelector("foo"))
    seq(2) should equal (ClassSelector("bar"))
    seq(3) should equal (AttributeSelector("href", Some(Beginning, "http")))
  }

  test("Universal selector can begin sequence of simple selectors") {
    val SingleSelector(seq, chain) = parse(singleSelector, "*.foo#bar")
    seq should have length (3)
    seq(0) should equal (UniversalSelector)
    seq(1) should equal (ClassSelector("foo"))
    seq(2) should equal (IdSelector("bar"))
  }

  test("Implicit universal selector can begin sequence of simple selectors") {
    val SingleSelector(seq, chain) = parse(singleSelector, ".foo.bar")
    seq should have length (3)
    seq(0) should equal (UniversalSelector)
    seq(1) should equal (ClassSelector("foo"))
    seq(2) should equal (ClassSelector("bar"))
  }

  test("Implicit universal selector must be followed by another simple selector") {
    intercept[ParsingException] { parse(singleSelector, "foo ++ bar") }
  }

  import Combinator._

  test("Descendant combinator produces corresponding nodes") {
    val SingleSelector(seq1, Some((Descendant, SingleSelector(seq2, _)))) =
      parse(singleSelector, "#foo .bar")
    seq1 should have length (2)
    seq1(0) should equal (UniversalSelector)
    seq1(1) should equal (IdSelector("foo"))
    seq2 should have length (2)
    seq2(0) should equal (UniversalSelector)
    seq2(1) should equal (ClassSelector("bar"))
  }

  test("Child combinator produces corresponding nodes") {
    val SingleSelector(seq1, Some((Child, SingleSelector(seq2, _)))) =
      parse(singleSelector, "foo > .bar")
    seq1 should have length (1)
    seq1(0) should equal (TypeSelector("foo"))
    seq2 should have length (2)
    seq2(0) should equal (UniversalSelector)
    seq2(1) should equal (ClassSelector("bar"))
  }

  test("Adjacent sibling combinator produces corresponding nodes") {
    val SingleSelector(seq1, Some((AdjacentSibling, SingleSelector(seq2, _)))) =
      parse(singleSelector, "foo+bar")
    seq1 should have length (1)
    seq1(0) should equal (TypeSelector("foo"))
    seq2 should have length (1)
    seq2(0) should equal (TypeSelector("bar"))
  }

  test("General sibling combinator produces corresponding nodes") {
    val SingleSelector(seq1, Some((GeneralSibling, SingleSelector(seq2, _)))) =
      parse(singleSelector, "#foo ~bar")
    seq1 should have length (2)
    seq1(0) should equal (UniversalSelector)
    seq1(1) should equal (IdSelector("foo"))
    seq2 should have length (1)
    seq2(0) should equal (TypeSelector("bar"))
  }

  test("Singleton selector group produces corresponding nodes") {
    val Selector(groups) = parse(selector, " * ")
    groups should have length (1)
    groups(0).sequence(0) should equal (UniversalSelector)
  }

  test("Selector group produces corresponding nodes") {
    val Selector(groups) = parse(selector, "foo , bar")
    groups should have length (2)
    groups(0).sequence(0) should equal (TypeSelector("foo"))
    groups(1).sequence(0) should equal (TypeSelector("bar"))
  }
}