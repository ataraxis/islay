package islay.transform

import scala.concurrent.Future
import scala.xml._
import scala.collection.LinearSeq


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

//  implicit object NodeSeqRenderer extends Renderer[NodeSeq] {
//    override def toNodeSeq(value: NodeSeq): Future[NodeSeq] = Future successful value
//  }
//
//  implicit object NodeBufferRenderer extends Renderer[NodeBuffer] {
//    override def toNodeSeq(value: NodeBuffer): Future[NodeSeq] = Future successful NodeSeq.fromSeq(value)
//  }

  /* Seq[Node] is the common superclass of NodeBuffer and NodeSeq. */
  implicit object SeqNodeRenderer extends Renderer[Seq[Node]] {
    override def toNodeSeq(value: Seq[Node]): Future[NodeSeq] = Future successful value
  }
//  implicit object NodeRenderer extends Renderer[Node] {
//    override def toNodeSeq(value: Node): Future[NodeSeq] = Future successful value
//  }

  implicit object RenderableRenderer extends Renderer[Renderable] {
    override def toNodeSeq(value: Renderable): Future[NodeSeq] = value.toNodeSeq
  }

  implicit def numericRenderer[A : Numeric]: Renderer[A] =
    new Renderer[A] {
      override def toNodeSeq(value: A): Future[NodeSeq] = Future successful Text(value.toString)
    }

  implicit def optionRenderer[A](implicit delegate: Renderer[A]): Renderer[Option[A]] =
    new Renderer[Option[A]] {
      override def toNodeSeq(value: Option[A]): Future[NodeSeq] =
        value.fold(Future successful NodeSeq.Empty)(delegate.toNodeSeq)
    }

//  implicit def traversableRenderer[A](implicit delegate: Renderer[A]): Renderer[Traversable[A]] = {
//    new Renderer[Traversable[A]] {
//      override def toNodeSeq(value: Traversable[A]): Future[NodeSeq] = {
//        import CallingThreadExecutor.Implicit
//        val seq = value.toIndexedSeq.map(delegate.toNodeSeq)
//        Future.sequence(seq).map(_.flatten)
//      }
//    }
//  }

  /* can't be more general than LinearSeq without creating ambiguous implicits for Node */
  implicit def linearSeqRenderer[A](implicit delegate: Renderer[A]): Renderer[LinearSeq[A]] = {
    new Renderer[LinearSeq[A]] {
      override def toNodeSeq(value: LinearSeq[A]): Future[NodeSeq] = {
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
}