package islay.transform
/*
import scala.language.experimental.macros
import scala.reflect.macros.{Context, TypecheckException}
import scala.tools.reflect.ToolBox


object NoncompilationTests {
  def compiles[T](code: T): Boolean = macro compilesImpl[T]
  def compilesImpl[T](c: Context)(code: c.Tree) = c.literal(
    try {
      c typeCheck code
      true
    } catch { case _:
      TypecheckException => false
    }
  )

  def eval(code: String, compileOptions: String = "-cp target/classes"): Any = {
    val tb = mkToolbox(compileOptions)
    tb.eval(tb.parse(code))
  }

  def mkToolbox(compileOptions: String = ""): ToolBox[_ <: scala.reflect.api.Universe] = {
    val m = scala.reflect.runtime.currentMirror
    m.mkToolBox(options = compileOptions)
  }
}
*/