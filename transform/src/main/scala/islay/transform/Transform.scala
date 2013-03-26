package islay.transform

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.xml.{Elem, NodeSeq}


class Transform(
    private val previous: Option[Transform] = None,
    private val operation: NodeSeq => Future[NodeSeq]
) extends Bindable {

  override def bindTo(elem: Elem): Future[NodeSeq] = {
    val newNodes = previous match {
      case None => Future.successful(elem)
      case Some(t) => t.bindTo(elem)
    }
    newNodes.flatMap(operation)(CallingThreadExecutor)
  }

  def apply(elem: Elem) = bindTo(elem)

  /**
   * For easy currying to `NodeSeq => NodeSeq`.
   */
  def synchronousApply(atMost: Duration)(elem: Elem): NodeSeq =
    Await.result(apply(elem), atMost)

  def &(next: Transform): Transform =
    new Transform(previous = Some(Transform.this), operation = next.operation)

  def when(condition: Boolean): Transform = {
    if (condition) Transform.this
    else new Transform(previous = previous, operation = Future.successful)
  }
}