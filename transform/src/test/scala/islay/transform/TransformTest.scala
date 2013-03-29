package islay.transform

import java.util.concurrent.Executors

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.DurationInt
import scala.xml.{Attribute, Null, Text}

import org.scalatest.FunSuite

import islay.transform.parser.{SingleSelector, UniversalSelector}


class TransformTest extends FunSuite {


  test("threads") {

    implicit val ec = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)

    val depth = 1420

    val xml = Range(1, depth).foldLeft(<div/>) { (child, _) =>
      <div/>.copy(child = child)
    }

    val selector = Selector(List(SingleSelector(List(UniversalSelector), None)))
//    val f = selector transform { elem =>
//      elem % Attribute("class", Text("smack"), Null)
//    } apply (xml)
    val f = selector.addAttr("class", "smack") apply (xml)

    val result = Await.result(f, 4.seconds)
//    println(result)
  }
}