package islay.transform

import scala.concurrent.ExecutionContext


/**
 * Improves `Future` performance when applying functions that are known to be non-blocking.
 */
object CallingThreadExecutor extends ExecutionContext {

  implicit val Implicit = this

  def reportFailure(t: Throwable) { throw t }
  def execute(runnable: Runnable) { runnable.run() }
}