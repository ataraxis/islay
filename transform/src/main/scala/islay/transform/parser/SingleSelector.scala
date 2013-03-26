package islay.transform.parser

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.{Elem, NodeSeq, Text}

import islay.transform.Transformation


object Selectors {
  type NodeReplacement = (Future[NodeSeq], Int)
}

import Selectors._

/**
 * A single selector in a comma-separated group of selectors. For convenience, the group of selectors is just
 * [[islay.transform.Selector]].
 */
case class SingleSelector(sequence: Seq[SimpleSelector], chain: Option[(Combinator, SingleSelector)]) {

  def hasGeneralSiblingChain: Boolean = chain.exists(_._1 == Combinator.GeneralSibling)

  def sequenceMatches(node: Elem, parent: Elem, index: Int) = sequence.forall(_.matches(node, parent, index))


  def apply(node: Elem, parent: Elem, index: Int, f: Transformation)
    (implicit executor: ExecutionContext): List[NodeReplacement] = {

    if (sequenceMatches(node, parent, index)) {
      chain match {
        case Some((combinator, selector)) =>
          combinator.apply(node, parent, index, selector, f)
        case _ =>
          List((f.apply(node), index))
      }
    }
    else
      Nil
  }
}


sealed abstract class SimpleSelector {
  def matches(node: Elem, parent: Elem, index: Int): Boolean
}

sealed abstract class HeadSelector extends SimpleSelector
sealed abstract class TailSelector extends SimpleSelector

case class TypeSelector(tag: String) extends HeadSelector {
  override def matches(node: Elem, parent: Elem, index: Int) = node.label == tag
}

case object UniversalSelector extends HeadSelector {
  override def matches(node: Elem, parent: Elem, index: Int) = true
}

case class AttributeSelector(name: String, matcher: Option[(AttributeOperator, String)]) extends TailSelector {
  override def matches(node: Elem, parent: Elem, index: Int) =
    node.attribute(name) exists {
      case Text(attr) =>
        matcher forall { case (op, value) =>
          op.matches(attr, value)
        }
      case _ => false
    }
}

case class ClassSelector(name: String) extends TailSelector {
  private val whitespace = " \t\n\r\f".r
  override def matches(node: Elem, parent: Elem, index: Int) =
    node.attribute("class") exists {
      case Text(value) => whitespace.split(value).contains(name)
      case _ => false
    }
}

case class IdSelector(id: String) extends TailSelector {
  private val text = Text(id)
  override def matches(node: Elem, parent: Elem, index: Int) = node.attribute("id").exists(text == _)
}

sealed abstract class AttributeOperator {
  def matches(attribute: String, value: String): Boolean
}

object AttributeOperator {
  case object ExactValue extends AttributeOperator {
    override def matches(attribute: String, value: String) = attribute == value
  }
  case object SpaceSeparated extends AttributeOperator {
    private val whitespace = " \t\n\r\f".r
    override def matches(attribute: String, value: String) = whitespace.split(attribute).contains(value)
  }
  case object FirstDash extends AttributeOperator {
    override def matches(attribute: String, value: String) = attribute == value || attribute.startsWith(value + "-")
  }
  case object Beginning extends AttributeOperator {
    override def matches(attribute: String, value: String) = attribute.startsWith(value)
  }
  case object Ending extends AttributeOperator {
    override def matches(attribute: String, value: String) = attribute.endsWith(value)
  }
  case object Substring extends AttributeOperator {
    override def matches(attribute: String, value: String) = attribute.contains(value)
  }
}

sealed abstract class PseudoClass extends TailSelector {
  override def matches(node: Elem, parent: Elem, index: Int) = false
}
case class Negation(sequence: Seq[SimpleSelector]) extends PseudoClass

sealed abstract class StructuralValue extends PseudoClass

object StructuralValue {
  case object Root extends StructuralValue
  case object FirstChild extends StructuralValue
  case object LastChild extends StructuralValue
  case object FirstOfType extends StructuralValue
  case object LastOfType extends StructuralValue
  case object OnlyOfType extends StructuralValue
  case object Empty extends StructuralValue
}