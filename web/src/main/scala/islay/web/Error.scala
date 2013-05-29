package islay.web

import java.util.Locale

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}
import scala.xml.NodeSeq

import islay.transform.Renderable


object Error {

  def apply(key: String, args: Any*)
      (implicit bundle: Bundle, locales: Seq[Locale], executor: ExecutionContext): Error = {
    new Error(Left(Message(key, args)))
  }

  def apply(message: NodeSeq): Error = {
    new Error(Right(message))
  }
}

class Error private (message: Either[Message, NodeSeq]) extends Renderable {

  override def toNodeSeq: Future[NodeSeq] = {
    message match {
      case Right(ns) => Future.successful(ns)
      case Left(m) => m.toNodeSeq
    }
  }

  def when(condition: Boolean): Error = ???

  def when(condition: Future[Boolean]): Error = ???
}