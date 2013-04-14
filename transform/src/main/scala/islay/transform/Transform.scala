package islay.transform

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.xml.{Elem, NodeSeq}


object Transform {

  /**
   * A transformation that returns the original element unmodified when applied. This is useful
   * when a value is needed to complete an expression, though `when()` can often be used instead.
   */
  val Noop = new Transform(operation = Future.successful)
}

class Transform(
    private val previous: Option[Transform] = None,
    private val operation: NodeSeq => Future[NodeSeq]
) extends Bindable {

  override def bindTo(elem: Elem): Future[NodeSeq] = apply(elem)

  def apply(nodes: NodeSeq): Future[NodeSeq] = {
    val newNodes = previous match {
      case None => Future.successful(nodes)
      case Some(t) => t.apply(nodes)
    }
    newNodes.flatMap(operation)(CallingThreadExecutor)
  }

  /**
   * For easy currying to `NodeSeq => NodeSeq`.
   */
  def synchronousApply(atMost: Duration)(nodes: NodeSeq): NodeSeq =
    Await.result(apply(nodes), atMost)

  /**
   * Compose this with another `Transform`. This transform will be applied first and the resulting
   * `NodeSeq` will be applied to the next transform.
   */
  def &(next: Transform): Transform =
    new Transform(previous = Some(this), operation = next.operation)

  /**
   * Returns a no-op transformation if the given condition is `false`, or returns itself if the
   * condition is `true`. For example:
   *
   * {{{
   * c"a".flatten when (!userHasEditPermissions)
   * }}}
   *
   * defines a transform that removes all links whenever `userHasEditPermissions` is false.
   */
  def when(condition: Boolean): Transform = {
    if (condition) this
    else new Transform(previous = previous, operation = Future.successful)
  }
}