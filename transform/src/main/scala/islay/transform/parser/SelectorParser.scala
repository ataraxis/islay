package islay.transform.parser

import org.parboiled.scala._
import org.parboiled.scala.rules.Rule0
import org.parboiled.scala.rules.Rule1
import org.parboiled.errors.ParsingException
import islay.transform.Selector


object SelectorParser extends SelectorParser

class SelectorParser extends Parser {
/*
  def PseudoClassSelector = rule { ":" ~ (StructuralValue | NthFunction | Negation) }

  def Negation: Rule0 = rule { ignoreCase("not(") ~ W ~ Sequence ~ W ~ ")" }

  def NthFunction = rule {
    (ignoreCase("nth-child") | ignoreCase("nth-last-child") | ignoreCase("nth-of-type") | ignoreCase("nth-last-of-type")) ~
    "(" ~ NthNotation ~ ")"
  }

  def NthNotation = rule { W ~ (
    optional(anyOf("-+")) ~ oneOrMore("0" - "9") ~ ignoreCase("n") ~ optional(W ~ anyOf("-+") ~ W ~ oneOrMore("0" - "9")) |
    optional(anyOf("-+")) ~ oneOrMore("0" - "9") |
    ignoreCase("odd") | ignoreCase("even")
  ) ~ W }
*/

  def selector: Rule1[Selector] = rule {
    oneOrMore(w ~ singleSelector ~ w, separator = ",") ~~> Selector ~ EOI
  }

  def singleSelector: Rule1[SingleSelector] = rule {
    sequence ~~> (_ +: _) ~ optional(combinator ~ singleSelector ~~> {(a, b) => (a, b)}) ~~> SingleSelector ~ w
  } label ("selector")

  def sequence: Rule2[HeadSelector, Seq[TailSelector]] = rule {
    (typeSelector | universalSelector) ~ zeroOrMore(tailSelector) |
    push(UniversalSelector) ~ oneOrMore(tailSelector)
  } label ("sequence of simple selectors")

  def tailSelector: Rule1[TailSelector] = rule {
    attributeSelector | classSelector | idSelector
  } label ("attribute, class, ID, or pseudo-class selector")

  def combinator: Rule1[Combinator] = rule { (w ~ (child | adjacentSibling | generalSibling) ~ w) | descendant }

  import Combinator._

  def descendant: Rule1[Combinator] = rule { oneOrMore(whitespace) ~ push(Descendant) } label ("descendant combinator")

  def child: Rule1[Combinator] = rule { ">" ~ push(Child) } label ("child combinator")

  def adjacentSibling: Rule1[Combinator] = rule { "+" ~ push(AdjacentSibling) } label ("adjacent sibling combinator")

  def generalSibling: Rule1[Combinator] = rule { "~" ~ push(GeneralSibling) } label ("general sibling combinator")

  def nthExpression = rule { w ~ (
    optional(anyOf("-+")) ~ oneOrMore("0" - "9") ~ ignoreCase("n") ~ optional(w ~ anyOf("-+") ~ w ~ oneOrMore("0" - "9")) |
    optional(anyOf("-+")) ~ oneOrMore("0" - "9") |
    ignoreCase("odd") | ignoreCase("even")
  ) ~> identity ~ w }


  def structuralValue: Rule1[StructuralValue] = rule {
    root | firstChild | lastChild | firstOfType | lastOfType | onlyOfType | empty | unknownStructuralValue
  }

  import StructuralValue._

  def root = rule { ignoreCase("root") ~ push(Root) }

  def firstChild = rule { ignoreCase("first-child") ~ push(FirstChild) }

  def lastChild = rule { ignoreCase("last-child") ~ push(LastChild) }

  def firstOfType = rule { ignoreCase("first-of-type") ~ push(FirstOfType) }

  def lastOfType = rule { ignoreCase("last-of-type") ~ push(LastOfType) }

  def onlyOfType = rule { ignoreCase("only-of-type") ~ push(OnlyOfType) }

  def empty = rule { ignoreCase("empty") ~ push(Empty) }

  def unknownStructuralValue: Rule1[StructuralValue] = rule {
    ANY ~> { name => throw new ParsingException("Unrecognized pseudo-class: "+ name) }
  }

  def typeSelector: Rule1[TypeSelector] = rule { identifier ~~> TypeSelector } label ("type selector")

  def universalSelector = rule { "*" ~ push(UniversalSelector) } label ("universal selector")

  def idSelector: Rule1[IdSelector] = rule { "#" ~ identifier ~~> IdSelector } label ("ID selector")

  def classSelector: Rule1[ClassSelector] = rule { "." ~ identifier ~~> ClassSelector } label ("class selector")

  def attributeSelector: Rule1[AttributeSelector] = rule {
    "[" ~ w ~ identifier ~ w ~ optional(attributeMatcher) ~ "]" ~~> AttributeSelector
  } label ("attribute selector")

  def attributeMatcher: Rule1[(AttributeOperator, String)] = rule {
    attributeOperator ~ w ~ (identifier | cssString) ~ w ~~> Pair.apply
  }

  def attributeOperator: Rule1[AttributeOperator] = rule {
    exactValue | spaceSeparated | firstDash | beginning | ending | substring | unknownOperator
  }

  import AttributeOperator._

  def exactValue: Rule1[AttributeOperator] = rule { "=" ~ push(ExactValue) }

  def spaceSeparated: Rule1[AttributeOperator] = rule { "~=" ~ push(SpaceSeparated) }

  def firstDash: Rule1[AttributeOperator] = rule { "|=" ~ push(FirstDash) }

  def beginning: Rule1[AttributeOperator] = rule { "^=" ~ push(Beginning) }

  def ending: Rule1[AttributeOperator] = rule { "$=" ~ push(Ending) }

  def substring: Rule1[AttributeOperator] = rule { "*=" ~ push(Substring) }

  def unknownOperator: Rule1[AttributeOperator] = rule { group(ANY ~ "=") ~> operatorError }

  def operatorError(op: String): AttributeOperator = throw new ParsingException("Unrecognized attribute operator: "+ op)

  def cssString: Rule1[String] = rule {
    "\"" ~ zeroOrMore(escapeSequence | noneOf("\n\r\f\"") ~> identity) ~~> (_.mkString) ~ "\"" |
    "'" ~ zeroOrMore(escapeSequence | noneOf("\n\r\f'") ~> identity) ~~> (_.mkString) ~ "'"
  }

  def identifier: Rule1[String] = rule { optional("-") ~> identity ~ identifierStartChar ~ identifierEnd ~~> (_+_+_) }

  def identifierStartChar: Rule1[String] = rule {
    ("a" - "z" | "A" - "Z" | "_" | "\u00a0" - "\ufffe") ~> identity | escapeSequence
  } label ("identifier starting character")

  def identifierEnd: Rule1[String] = rule { zeroOrMore(identifierChar) ~~> (_.mkString) }

  def identifierChar: Rule1[String] = rule { ("-" | "0" - "9") ~> identity | identifierStartChar }

  def escapeSequence = rule { "\\" ~ (
    hex ~ nTimes(5, optional(hex)) ~ whitespace |
    nTimes(6, hex) ~ optional(whitespace) |
    !(hex | anyOf("\n\r\f")) ~ ANY |
    EOI
  ) ~> unescape }

  def hex = rule { "a" - "f" | "A" - "F" | "0" - "9" }

  def w = rule { zeroOrMore(whitespace) } label ("whitespace")

  def whitespace = rule { anyOf(" \t\n\r\f") }


  def unescape(s: String): String = {
    val v = s.trim
    if (v.isEmpty) "\\"
    else try {
      val i = Integer.parseInt(v, 16)
      if (i > 0xfffd)
        throw new ParsingException("Escape code cannot be greater than \\00fffd")
      else
        Character.toChars(i)(0).toString
    }
    catch { case _: NumberFormatException =>
      v
    }
  }
}