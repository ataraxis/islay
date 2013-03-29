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

  test("<> binds another transform with matched element as root") {

    val transform = pSelector <> (rootSelector <> "++")
    val f = transform(<div>Foo<p>Bar</p>Baz</div>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("div")
    result(0).text should be ("Foo++Baz")
  }

  test("<> binds multiple transforms with matched element as root") {

    val transform = pSelector <> Seq(
      rootSelector <> "+",
      rootSelector <> "-"
    )
    val f = transform(<div>Foo<p>Bar</p>Baz</div>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("div")
    result(0).text should be ("Foo+-Baz")
  }

  test("`remove` removes matched element and descendants") {

    val transform = pSelector.remove
    val f = transform(<div>Foo<p>Bar<baz>Bux</baz></p>Baz</div>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("div")
    result(0).text should be ("FooBaz")
  }

  test("`addAttr` replaces an existing attribute on a matched element") {

    val transform = pSelector.addAttr("class", "boff")
    val f = transform(<p class="biff"></p>)
    val result = Await.result(f, 2.seconds)

    result(0).attribute("class") should equal (Some(Text("boff")))
  }

  test("`removeAttr` removes an attribute from a matched element") {

    val transform = pSelector.removeAttr("class")
    val f = transform(<p class="bang"></p>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("p")
    result(0).attribute("class") should be (None)
  }

  test("`addClass` appends a class name to an existing class attribute") {

    val transform = pSelector.addClass("boff bash")
    val f = transform(<p class="crash bang"></p>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("p")
    result(0).attribute("class") should equal (Some(Text("crash bang boff bash")))
  }

  test("`addClass` creates a class attribute if it doesn't already exist") {

    val transform = pSelector.addClass("boff")
    val f = transform(<p></p>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("p")
    result(0).attribute("class") should equal (Some(Text("boff")))
  }

  test("`removeClass` removes only specified classes from class attribute") {

    val transform = pSelector.removeClass("crash boff")
    val f = transform(<p class="boff bish crash crump"></p>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("p")
    result(0).attribute("class") should equal (Some(Text("bish crump")))
  }

  test("`before` inserts content before a matched element") {

    val transform = pSelector.before(Some(47))
    val f = transform(<p/>)
    val result = Await.result(f, 2.seconds)

    result(0) should equal (Text("47"))
    result(1).label should be ("p")
  }

  test("`after` inserts content after a matched element") {

    val transform = pSelector.after(List("a", "b"))
    val f = transform(<p/>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("p")
    result(1) should equal (Text("a"))
    result(2) should equal (Text("b"))
  }

  test("`prepend` inserts content before the children of a matched element") {

    val transform = pSelector.prepend(Future.successful("Twinkie."))
    val f = transform(<p>Slime.</p>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("p")
    result(0).child(0) should equal (Text("Twinkie."))
    result(0).child(1) should equal (Text("Slime."))
  }

  test("`append` inserts content after the children of a matched element") {

    val transform = pSelector.append(List(10, 7.1))
    val f = transform(<p><span/></p>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("p")
    result(0).child(0).label should be ("span")
    result(0).child(1) should equal (Text("10.0"))
    result(0).child(2) should equal (Text("7.1"))
  }
}