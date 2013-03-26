package islay.transform.parser

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.xml.Elem

import Selectors.NodeReplacement
import islay.transform.{CallingThreadExecutor, TransformFunction}


sealed abstract class Combinator {

  /**
   * Applies the given selector (following its chain if it has one) to the given node and returns the replacements
   * that should be applied to parent (in order of descending index).
   */
  def apply(node: Elem, parent: Elem, index: Int, selector: SingleSelector, f: TransformFunction)
    (implicit executor: ExecutionContext): List[NodeReplacement] = ???


  protected def applyToChildren(node: Elem, selector: SingleSelector, f: TransformFunction)
      (implicit executor: ExecutionContext): Option[Future[Elem]] = {

    val children = node.child

    @tailrec
    def loop(i: Int, replacements: List[NodeReplacement]): List[NodeReplacement] = {
      if (i >= children.length)
        replacements
      else {
        children(i) match {
          case child: Elem =>
            val newReplacements = selector.apply(child, node, i, f) ::: replacements
            if (selector.hasGeneralSiblingChain)
              newReplacements
            else
              loop(i+1, newReplacements)
          case _ =>
            loop(i+1, replacements)
        }
      }
    }

    val replacements = loop(0, Nil)
    replaceChildren(node, replacements)
  }

  protected def replaceChildren(parent: Elem, replacements: List[NodeReplacement])
      (implicit executor: ExecutionContext): Option[Future[Elem]] = {

    if (replacements.isEmpty)
      None
    else {
      var remaining = replacements.reverse
      val fetuses = Future.traverse(parent.child.zipWithIndex) { case (node, i) =>
        remaining match {
          case (replacement, index) :: tail if index == i =>
            remaining = tail
            replacement
          case _ =>
            Future.successful(node)
        }
      }
      Some(fetuses.map(fs => parent.copy(child = fs.flatten)))
    }
  }
}

object Combinator {

  /**
   * A selector of the form "A B" represents an element B that is an arbitrary descendant of some ancestor element A.
   */
  case object Descendant extends Combinator {

    override def apply(cursor: Elem, parent: Elem, index: Int, selector: SingleSelector, f: TransformFunction)
        (implicit executor: ExecutionContext): List[NodeReplacement] = {
      apply(cursor, selector, f) match {
        case Some(updated) => List((updated, index))
        case None => Nil
      }
    }

    def apply(node: Elem, selector: SingleSelector, f: TransformFunction)
        (implicit executor: ExecutionContext): Option[Future[Elem]] = {

      descendChildren(node, selector, f) map {
        import CallingThreadExecutor.Implicit
        _ flatMap { newNode =>
          applyToChildren(newNode, selector, f)(executor) getOrElse Future.successful(newNode)
        }
      } orElse {
        applyToChildren(node, selector, f)
      }
    }

    def descendChildren(node: Elem, selector: SingleSelector, f: TransformFunction)
        (implicit executor: ExecutionContext): Option[Future[Elem]] = {

      val children = node.child

      @tailrec
      def loop(i: Int, replacements: List[NodeReplacement]): List[NodeReplacement] = {
        if (i >= children.length)
          replacements
        else {
          val newReplacements = children(i) match {
            case child: Elem =>
              this.apply(child, selector, f).fold(replacements) {
                (_, i) :: replacements
              }
            case _ => replacements
          }
          loop(i+1, newReplacements)
        }
      }

      val replacements = loop(0, Nil)
      replaceChildren(node, replacements)
    }
  }

  /**
   * A selector of the form "A > B" represents an element B that is an immediate child of some parent element A.
   */
  case object Child extends Combinator {

    override def apply(cursor: Elem, parent: Elem, index: Int, selector: SingleSelector, f: TransformFunction)
        (implicit executor: ExecutionContext): List[NodeReplacement] = {

      applyToChildren(cursor, selector, f) match {
        case Some(updated) => List((updated, index))
        case None => Nil
      }
    }
  }

  /**
   * A selector of the form "A + B" represents an element B that is immediately following some element A.
   */
  case object AdjacentSibling extends Combinator {

    override def apply(node: Elem, parent: Elem, index: Int, selector: SingleSelector, f: TransformFunction)
        (implicit executor: ExecutionContext): List[NodeReplacement] = {

      val siblings = parent.child

      @tailrec
      def applyNext(i: Int): List[NodeReplacement] = {
        if (i >= siblings.length)
          Nil
        else {
          siblings(i) match {
            case sibling: Elem =>
              selector.apply(sibling, parent, i, f)
            case _ =>
              applyNext(i+1)
          }
        }
      }
      applyNext(index+1)
    }
  }

  /**
   * A selector of the form "A ~ B" represents an element B that follows (not necessarily immediately) some element A.
   */
  case object GeneralSibling extends Combinator {

    override def apply(node: Elem, parent: Elem, index: Int, selector: SingleSelector, f: TransformFunction)
        (implicit executor: ExecutionContext): List[NodeReplacement] = {

      val siblings = parent.child

      @tailrec
      def loop(i: Int, replacements: List[NodeReplacement]): List[NodeReplacement] = {
        if (i >= siblings.length)
          replacements
        else {
          val newReplacements = siblings(i) match {
            case sibling: Elem =>
              selector.apply(sibling, parent, i, f) ::: replacements
            case _ =>
              replacements
          }
          loop(i+1, newReplacements)
        }
      }
      loop(index+1, Nil)
    }
  }
}