package islay.transform

import scala.xml.Elem
import scala.concurrent.Future
import scala.xml.NodeSeq


abstract class Binder[-A] {
  def bind(value: A, elem: Elem): Future[NodeSeq]
}

object Binder {

  implicit object BindableBinder extends Binder[Bindable] {
    override def bind(value: Bindable, elem: Elem): Future[NodeSeq] = value.bindTo(elem)
  }

//  implicit object OptionBinder extends Binder[Option[Bindable]] {
//    override def bind(value: Option[Bindable], elem: Elem): Future[NodeSeq] =
//      value.fold(Future successful NodeSeq.Empty)(_.bindTo(elem))
//  }
//
  implicit object TraversableBinder extends Binder[Traversable[Bindable]] {
    override def bind(value: Traversable[Bindable], elem: Elem): Future[NodeSeq] = {
      import CallingThreadExecutor.Implicit
      val seq = value.toIndexedSeq.map(_.bindTo(elem))
      Future.sequence(seq).map(_.flatten)
    }
  }

  implicit def rendererBinder[A](implicit delegate: Renderer[A]): Binder[A] =
    new Binder[A] {
      override def bind(value: A, elem: Elem): Future[NodeSeq] =
        delegate.toNodeSeq(value)
    }

  implicit def optionBinder[A](implicit delegate: Binder[A]): Binder[Option[A]] =
    new Binder[Option[A]] {
      override def bind(value: Option[A], elem: Elem): Future[NodeSeq] =
        value.fold(Future successful NodeSeq.Empty)(delegate.bind(_, elem))
    }

//  implicit def seqBinder[A](implicit delegate: Binder[A]): Binder[Seq[A]] =
//    new Binder[Seq[A]] {
//      override def bind(value: Seq[A], elem: Elem): Future[NodeSeq] = {
//        import CallingThreadExecutor.Implicit
//        val seq = value.toIndexedSeq.map(delegate.bind(_, elem))
//        Future.sequence(seq).map(_.flatten)
//      }
//    }
}