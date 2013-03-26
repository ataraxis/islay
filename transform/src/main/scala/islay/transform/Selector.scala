package islay.transform

import scala.concurrent.{ExecutionContext, Future}
import scala.xml._
import islay.transform.parser.{Combinator, SingleSelector}
import scala.annotation.tailrec


/**
 * Since every selector starts with an implicit descendant combinator starting from an invisible element at the root
 * of its tree, this node serves as the element from which that combinator can descend. It also simplifies things by
 * ensuring every node has a parent.
 */
private[islay] case class GodNode(children: NodeSeq) extends Elem(null, "GOD", Null, TopScope, false, children: _*)


case class Selector(private val groups: Seq[SingleSelector]) {

  /**
   * Applies a transformation function to the elements matched by this selector against a given `NodeSeq`, returning
   * an updated `NodeSeq`.
   */
  def apply(nodes: NodeSeq)(f: TransformFunction)
      (implicit executor: ExecutionContext): Future[NodeSeq] = {

    groups.foldLeft(Future successful nodes) { (acc, selector) =>
      acc flatMap { roots =>
        val god = GodNode(roots)
        val maybeNewGod = Combinator.Descendant.apply(god, selector, f)
        maybeNewGod.fold(acc)(_.map(_.child))
      }
    }
  }

  def transform(f: Elem => NodeSeq)(implicit executor: ExecutionContext): Transform =
    futureTransform(f andThen Future.successful)

  def futureTransform(f: TransformFunction)(implicit executor: ExecutionContext): Transform =
    new Transform(
      operation = ns => apply(ns)(f)
    )

  private def innerTransform(f: Elem => NodeSeq) =
    transform(f)(CallingThreadExecutor)

  private def innerFutureTransform(f: TransformFunction) =
    futureTransform(f)(CallingThreadExecutor)

  /**
   * Returns the nodes matched by this selector on a given `NodeSeq`.
   */
  def select(ns: NodeSeq): NodeSeq = {
    /* TODO: needs more efficient solution */
    var result: Seq[Elem] = Nil
    innerTransform { elem =>
      result :+= elem
      elem
    }
    result
  }

  /**
   * Defines a [[islay.transform.Transform]] that replaces the content (children) of every element matched by this
   * selector with new content. The new content must be renderable as XML through a [[islay.transform.Renderer]]
   * typeclass instance.
   *
   * This corresponds to jQuery's [[http://api.jquery.com/html/#html2 html()]] method.
   */
  def **[T](content: T)(implicit renderer: Renderer[T]): Transform = {
    innerFutureTransform { elem =>
      import CallingThreadExecutor.Implicit
      renderer.toNodeSeq(content) map { ns =>
        elem.copy(child = ns)
      }
    }
  }

  def **(content: Node): Transform = ???
  /* XXX: this method exists to work around the diverging implicit expansion that occurs doing: `t ** <foo/>` */

//  def **(content: Traversable[NodeSeq]): Transform = ???

  /**
   * Defines a [[islay.transform.Transform]] that _binds_ the supplied content to every element matched by this
   * selector. The content may be an instance of [[islay.transform.Bindable]], in which case it implements a
   * transformation function that produces a new structure from the matched element, or the content may be anything
   * renderable as XML (through a [[islay.transform.Renderer]] typeclass instance), in which case the matched element
   * is simply replaced by the new content.
   *
   * If the supplied content is another `Transform`, the nested transform will have as its root the matched element of
   * this selector. For example
   *
   * `c".my-class" <> (c"p" <> "foo")`
   *
   * is equivalent to
   *
   * `c".my-class p" <> "foo"`
   *
   * This method corresponds to jQuery's [[http://api.jquery.com/replaceWith/#replaceWith-function replaceWith()]] method.
   */
  def <>[T](content: T)(implicit binder: Binder[T]): Transform = {
    innerFutureTransform { elem =>
      binder.bind(content, elem)
    }
  }

  def remove: Transform = innerTransform(_ => NodeSeq.Empty)

  def empty: Transform = innerTransform(_.copy(child = Nil))

  /**
   * Defines a [[islay.transform.Transform]] that adds (or replaces) an attribute on every element matched by this
   * selector.
   */
  def addAttr(name: String, value: String): Transform = innerTransform { elem =>
    elem % Attribute(name, Text(value), Null)
  }

  def addAttr(nameValue: Future[(String, String)]): Transform =
    innerFutureTransform { elem =>
      import CallingThreadExecutor.Implicit
      nameValue map { case (name, value) =>
        elem % Attribute(name, Text(value), Null)
      }
    }

  def removeAttr(name: String): Transform = ???

  def addClass(name: String): Transform = ???

  def removeClass(name: String): Transform = ???

  def before[T](content: T)(implicit renderer: Renderer[T]): Transform = ???

  def after[T](content: T)(implicit renderer: Renderer[T]): Transform = ???

  def prepend[T](content: T)(implicit renderer: Renderer[T]): Transform = ???

  def append[T](content: T)(implicit renderer: Renderer[T]): Transform = ???

  /**
   * Defines a [[islay.transform.Transform]] that wraps an XML structure around every element matched by this selector.
   * The structure may be nested, in which case the matched element is placed at the bottom of this structure.
   *
   * The precise wrapping behavior matches that of jQuery's [[http://api.jquery.com/wrap/#wrap-wrappingElement wrap()]]
   * method.
   */
  def wrap(wrapper: Elem): Transform = innerTransform { elem =>

    def wrap(wrapper: Elem): Elem = {
      wrapper.child match {
        case (head: Elem) +: tail =>
          wrapper.copy(child = wrap(head) +: tail)
        case _ =>
          wrapper.copy(child = wrapper.child ++ elem)
      }
    }
    wrap(wrapper)
  }

  def wrapInner(elem: Elem): Transform = ???

  def unwrap: Transform = ???
}