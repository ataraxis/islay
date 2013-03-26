package islay.transform

import org.scalatest.FunSuite
import islay.transform._
import islay.transform.parser._
import org.scalatest.matchers.ShouldMatchers
import scala.concurrent.Future


/**
 * Note: this test is here rather than in the transform project because a macro and its uses cannot be defined in the
 * same compilation unit.
 */
class SelectorMacroTest extends FunSuite with ShouldMatchers {

  ignore("whatever") ({
    c"a + b" should (equal (Selector(Nil)))

    c"a + b" <> Some((c"b *" <> List("Hello"))) &
    (c"foo" ** "bar" when (true))
  })
}