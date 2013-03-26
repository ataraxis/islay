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


class SelectorTest extends FunSuite with ShouldMatchers {

  implicit val executor = CallingThreadExecutor //ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)

  val universalSelector = Selector(List(SingleSelector(List(UniversalSelector), None)))
  val pSelector = Selector(List(SingleSelector(TypeSelector("p") :: Nil, None)))


  test("** replaces children") {

    var transform: Transform = null
//    transform = universalSelector ** List(42, "foo")
    transform = universalSelector ** List(<foo></foo><bar/>, <bar/>)
//    transform = universalSelector ** <foo></foo><bar/><bar/>;
//    transform = universalSelector ** <foo/>
    transform = universalSelector ** Future(Seq(<foo/>).flatten)

//    val transform = universalSelector ** List(<foo></foo><bar/>, <bar/>);
//    val f = transform(<foo>Bar<bar>Baz</bar></foo>)
//    val result = Await.result(f, 2.seconds)
//    println(result)

//    val transform = universalSelector ** List(Future(Some(42)))
    val f = transform(<foo>Bar<bar>Baz</bar></foo>)
    val result = Await.result(f, 2.seconds)
    println("result = "+result)
//
//    result(0).label should be ("foo")
//    result(0).text should be ("42")
  }

  test("<> replaces elements with renderable") {

    val transform = pSelector <> List(Future(Some(", ")))
    val f = transform(<div>Foo<p>Bar</p>Baz</div>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("div")
    result(0).text should be ("Foo, Baz")
  }

  test("<> replaces elements with all renderables in a list") {

    val transform = pSelector <> List("", "")
    val f = transform(<div>Foo<p>Bar</p>Baz</div>)
    val result = Await.result(f, 2.seconds)

    result(0).label should be ("div")
    result(0).text should be ("Foo, Baz")
  }
}