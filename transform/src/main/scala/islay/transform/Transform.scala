package islay.transform

import scala.xml.NodeSeq
import scala.xml.Elem
import scala.concurrent.Future
import scala.concurrent.ExecutionContext


class Transform(
    private val previous: Option[Transform] = None,
    private val operation: NodeSeq => Future[NodeSeq]
) extends Bindable {

  override def bindTo(elem: Elem): Future[NodeSeq] = {
    val newNodes = previous match {
      case None => Future.successful(elem)
      case Some(t) => t.bindTo(elem)
    }
    println("elem = "+elem)
    newNodes.flatMap(operation)(CallingThreadExecutor)
  }

  def apply(elem: Elem) = bindTo(elem)

  def &(next: Transform): Transform =
    new Transform(previous = Some(Transform.this), operation = next.operation)

  def when(condition: Boolean): Transform = {
    if (condition) Transform.this
    else new Transform(previous = previous, operation = Future.successful)
  }
}