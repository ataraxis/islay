package islay.transform

import scala.concurrent.ExecutionContext


/**
 * Improves `Future` performance when applying functions that are known to be non-blocking.
 */
object CallingThreadExecutor extends ExecutionContext {

  implicit val Implicit = this

  def reportFailure(t: Throwable) {
    Thread.getDefaultUncaughtExceptionHandler match {
      case null => t.printStackTrace()
      case handler => handler.uncaughtException(Thread.currentThread, t)
    }
  }

  def execute(runnable: Runnable) {
    try {
      runnable.run()
    } catch { case t: Throwable =>
      reportFailure(t)
    }
  }
}