package islay.transform

import scala.concurrent.Future
import scala.xml._


/**
 * Represents a strategy for converting values of a particular type into a `Future[NodeSeq]`
 * which can then be composed generically into a transformation function.
 */
abstract class Renderer[-A] {
  def toNodeSeq(value: A): Future[NodeSeq]
}

/**
 * Allows custom objects to be rendered without needing their own Renderer typeclass instance.
 */
trait Renderable {
  def toNodeSeq: Future[NodeSeq]
}

object Renderer {

  implicit object StringRenderer extends Renderer[CharSequence] {
    override def toNodeSeq(value: CharSequence): Future[NodeSeq] = Future successful Text(value.toString)
  }

  implicit object RenderableRenderer extends Renderer[Renderable] {
    override def toNodeSeq(value: Renderable): Future[NodeSeq] = value.toNodeSeq
  }

  implicit def numericRenderer[A : Numeric]: Renderer[A] =
    new Renderer[A] {
      override def toNodeSeq(value: A): Future[NodeSeq] = Future successful Text(value.toString)
    }

  implicit def nodeRenderer[A](implicit delegate: NodeRenderer[A]): Renderer[A] =
    new Renderer[A] {
      override def toNodeSeq(value: A): Future[NodeSeq] = Future successful delegate.toNodeSeq(value)
    }

  implicit def optionRenderer[A](implicit delegate: Renderer[A]): Renderer[Option[A]] =
    new Renderer[Option[A]] {
      override def toNodeSeq(value: Option[A]): Future[NodeSeq] =
        value.fold(Future successful NodeSeq.Empty)(delegate.toNodeSeq)
    }

  implicit def traversableRenderer[A](implicit delegate: Renderer[A]): Renderer[Traversable[A]] = {
    new Renderer[Traversable[A]] {
      override def toNodeSeq(value: Traversable[A]): Future[NodeSeq] = {
        import CallingThreadExecutor.Implicit
        val seq = value.toIndexedSeq.map(delegate.toNodeSeq)
        Future.sequence(seq).map(_.flatten)
      }
    }
  }

  implicit def futureRenderer[A](implicit delegate: Renderer[A]): Renderer[Future[A]] =
    new Renderer[Future[A]] {
      override def toNodeSeq(value: Future[A]): Future[NodeSeq] =
        value.flatMap(delegate.toNodeSeq)(CallingThreadExecutor)
  }

  /* XXX: this is a hack to work around the ambiguous implicits error that occurs when renderers
   * exist for both Node and Traversable and a NodeSeq is given */
  abstract class NodeRenderer[-A] {
    def toNodeSeq(value: A): NodeSeq
  }

  object NodeRenderer {
    implicit object Instance extends NodeRenderer[Node] {
      override def toNodeSeq(value: Node): NodeSeq = value
    }
  }
}