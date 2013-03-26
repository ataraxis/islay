package islay.transform

import scala.concurrent.ExecutionContext


object CallingThreadExecutor extends ExecutionContext {

  implicit val Implicit = this

  def reportFailure(t: Throwable) { throw t }
  def execute(runnable: Runnable) { runnable.run() }
}