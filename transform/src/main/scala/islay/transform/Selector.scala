package islay.transform

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.xml._

import islay.transform.parser.{Combinator, SingleSelector}


/**
 * Since every selector starts with an implicit descendant combinator starting from an invisible
 * element at the root of its tree, this node serves as the element from which that combinator can
 * descend. It also simplifies things by ensuring every node has a parent.
 */
private[islay] case class GodNode(children: NodeSeq) extends Elem(null, "GOD", Null, TopScope, false, children: _*)


case class Selector(private val groups: Seq[SingleSelector]) {

  /**
   * Applies a transformation function to the elements matched by this selector against a given
   * `NodeSeq`, returning an updated `NodeSeq`.
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
   * Defines a [[islay.transform.Transform]] that replaces the content (children) of every element
   * matched by this selector with new content. The new content must be renderable as XML through a
   * [[islay.transform.Renderer]] typeclass instance.
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

  /**
   * Defines a [[islay.transform.Transform]] that _binds_ the supplied content to every element
   * matched by this selector. The content may be an instance of [[islay.transform.Bindable]], in
   * which case it implements a transformation function that produces a new structure from the
   * matched element, or the content may be anything renderable as XML (through a
   * [[islay.transform.Renderer]] typeclass instance), in which case the matched element is simply
   * replaced by the new content.
   *
   * If the supplied content is another `Transform`, the nested transform will have as its root the
   * matched element of this selector. For example
   *
   * `c".my-class" <> (c"p" <> "foo")`
   *
   * is equivalent to
   *
   * `c".my-class p" <> "foo"`
   *
   * This method corresponds to jQuery's
   * [[http://api.jquery.com/replaceWith/#replaceWith-function replaceWith()]] method.
   */
  def <>[T](content: T)(implicit binder: Binder[T]): Transform = {
    innerFutureTransform { elem =>
      binder.bind(content, elem)
    }
  }

  /**
   * Defines a [[islay.transform.Transform]] that removes every element matched by this selector
   * and all of its descendants.
   */
  def remove: Transform = innerTransform(_ => NodeSeq.Empty)

  /**
   * Defines a [[islay.transform.Transform]] that removes all descendants of every element matched
   * by this selector.
   */
  def empty: Transform = innerTransform(_.copy(child = Nil))

  /**
   * Defines a [[islay.transform.Transform]] that adds (or replaces) an attribute on every element
   * matched by this selector.
   */
  def addAttr(name: String, value: String): Transform = innerTransform { elem =>
    elem % Attribute(name, Text(value), Null)
  }

  /**
   * Defines a [[islay.transform.Transform]] that removes an attribute on every element matched by
   * this selector.
   */
  def removeAttr(name: String): Transform = innerTransform { elem =>
    elem % Attribute(name, null, Null)
  }

  /**
   * Defines a [[islay.transform.Transform]] that adds a class name (or multiple space-separated
   * class names) to every element matched by this selector. More precisely, if a "class" attribute
   * doesn't exist then it is added with the value specified, otherwise the value specified is
   * appended to the current value with some whitespace thrown in.
   *
   * This method does not replace any existing classes with the same name, it only appends to the
   * end of the class attribute.
   */
  def addClass(name: String): Transform = innerTransform { elem =>
    val value = elem.attribute("class") match {
      case Some(Text(current)) => current +" "+ name
      case _ => name
    }
    elem % Attribute("class", Text(value), Null)
  }

  /**
   * Defines a [[islay.transform.Transform]] that removes a class name (or multiple space-separated
   * class names) on every element matched by this selector. More precisely, if a "class" attribute
   * exists then all specified names appearing within the list of space-separated of tokens in the
   * attribute value will be removed from that value.
   */
  def removeClass(name: String): Transform = innerTransform { elem =>
    elem.attribute("class") match {
      case Some(Text(value)) =>
        val currentNames = value.split("\\s+").toBuffer
        val newNames = name.split("\\s+").foldLeft(currentNames)(_ - _)
        elem % Attribute("class", Text(newNames mkString " "), Null)
      case _ =>
        elem
    }
  }

  /**
   * Defines a [[islay.transform.Transform]] that inserts the specified content before each
   * element matched by this selector.
   */
  def before[T](content: T)(implicit renderer: Renderer[T]): Transform = innerFutureTransform { elem =>
    renderer.toNodeSeq(content).map(_ ++ elem)(CallingThreadExecutor)
  }

  /**
   * Defines a [[islay.transform.Transform]] that inserts the specified content after each element
   * matched by this selector.
   */
  def after[T](content: T)(implicit renderer: Renderer[T]): Transform = innerFutureTransform { elem =>
    renderer.toNodeSeq(content).map(elem ++ _)(CallingThreadExecutor)
  }

  /**
   * Defines a [[islay.transform.Transform]] that inserts the specified content to the beginning of
   * the child list of each element matched by this selector.
   */
  def prepend[T](content: T)(implicit renderer: Renderer[T]): Transform = innerFutureTransform { elem =>
    import CallingThreadExecutor.Implicit
    renderer.toNodeSeq(content) map { ns =>
      elem.copy(child = ns ++ elem.child)
    }
  }

  /**
   * Defines a [[islay.transform.Transform]] that inserts the specified content to the end of the
   * child list of each element matched by this selector.
   */
  def append[T](content: T)(implicit renderer: Renderer[T]): Transform = innerFutureTransform { elem =>
    import CallingThreadExecutor.Implicit
    renderer.toNodeSeq(content) map { ns =>
      elem.copy(child = elem.child ++ ns)
    }
  }

  /**
   * Defines a [[islay.transform.Transform]] that wraps an XML structure around every element
   * matched by this selector. The structure may be nested, in which case the matched element
   * is placed at the bottom of this structure.
   *
   * The precise wrapping behavior matches that of jQuery's
   * [[http://api.jquery.com/wrap/#wrap-wrappingElement wrap()]] method.
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

  /**
   * Defines a [[islay.transform.Transform]] that wraps an XML structure around the child elements
   * of every element matched by this selector. The structure may be nested, in which case the
   * children are placed at the bottom of this structure.
   *
   * The precise wrapping behavior matches that of jQuery's
   * [[http://api.jquery.com/wrapInner/#wrapInner-wrappingElement wrapInner()]] method.
   */
  def wrapInner(wrapper: Elem): Transform = innerTransform { elem =>

    def wrap(wrapper: Elem): Elem = {
      wrapper.child match {
        case (head: Elem) +: tail =>
          wrapper.copy(child = wrap(head) +: tail)
        case _ =>
          wrapper.copy(child = wrapper.child ++ elem.child)
      }
    }
    elem.copy(child = wrap(wrapper))
  }

  /**
   * Defines a [[islay.transform.Transform]] that substitutes every element matched by this
   * selector with its children.
   */
  def flatten: Transform = innerTransform(_.child)
}