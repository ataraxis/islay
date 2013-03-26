package islay.transform

import java.util.concurrent.Executors

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.xml.{Attribute, Null, Text}

import org.scalatest.FunSuite

import islay.transform.parser.{SingleSelector, UniversalSelector}


class TransformTest extends FunSuite {


  ignore("threads") {

    implicit val ec = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor)

    val selector = Selector(List(SingleSelector(List(UniversalSelector), None)))
    selector transform { elem =>
      elem % Attribute("class", Text("smack"), Null)
    } apply (<foo></foo>)

//    val t = selector ** List(Future{Thread.sleep(3000);Some(42)})
//    val f = t.apply(<foo>Bar</foo>)
//    val result = Await.result(f, 4.seconds)
//    println(result)
  }
}