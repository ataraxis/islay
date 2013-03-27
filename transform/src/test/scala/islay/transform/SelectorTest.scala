package islay.transform

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.xml.{Attribute, Null, Text}
import org.scalatest.FunSuite
import islay.transform.parser.{SingleSelector, UniversalSelector}
import org.scalatest.matchers.ShouldMatchers
import islay.transform.parser.TypeSelector
import scala.xml.NodeSeq
import islay.transform.parser.StructuralValue


class SelectorTest extends FunSuite with ShouldMatchers {

  implicit val executor = CallingThreadExecutor //ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)

  val universalSelector = Selector(List(SingleSelector(List(UniversalSelector), None)))
  val pSelector = Selector(List(SingleSelector(TypeSelector("p") :: Nil, None)))
  val rootSelector = Selector(List(SingleSelector(StructuralValue.Root :: Nil, None)))

  test("** replaces children") {

    val transform = universalSelector ** List(Future(Some(42)))
    val f = transform(<foo>Bar<bar>Baz</bar></foo>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("foo")
    result(0).text should be ("42")
  }

  test("** with None removes children") {

    val transform = universalSelector ** (None: Option[Int])
    val f = transform(<foo>Bar<bar>Baz</bar></foo>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("foo")
    result(0).child should have length (0)
  }

  test("<> replaces elements with renderable") {

    val transform = pSelector <> List(Future(Some(", ")))
    val f = transform(<div>Foo<p>Bar</p>Baz</div>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("div")
    result(0).text should be ("Foo, Baz")
  }

  test("<> replaces elements with all renderables in a list") {

    val transform = pSelector <> Seq("Bo", "ff")
    val f = transform(<div>Foo<p>Bar</p>Baz</div>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("div")
    result(0).text should be ("FooBoffBaz")
  }

  test("<> binds another transform with matched node as root") {

    val transform = pSelector <> (rootSelector <> "++")
    val f = transform(<div>Foo<p>Bar</p>Baz</div>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("div")
    result(0).text should be ("Foo++Baz")
  }

  test("<> binds multiple transforms with matched node as root") {

    val transform = pSelector <> Seq(
      rootSelector <> "+",
      rootSelector <> "-"
    )
    val f = transform(<div>Foo<p>Bar</p>Baz</div>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("div")
    result(0).text should be ("Foo+-Baz")
  }
}