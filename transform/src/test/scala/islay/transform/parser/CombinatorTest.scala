package islay.transform.parser

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.xml.{Attribute, Elem, NodeSeq}
import scala.xml.{Null, Text}
import scala.xml.NodeSeq.seqToNodeSeq

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import Combinator._
import islay.transform.Selector


class CombinatorTest extends FunSuite with ShouldMatchers {

  implicit val executor = ExecutionContext.Implicits.global

  val xml = <div id="a">
              <div id="b"></div>
              <div id="c">
                <p id="d">Wibble</p>
                <p id="e">Wobble</p>
                <p id="f">Wubble</p>
                <div id="g">
                  <span id="h">Watusi</span>
                </div>
                <p id="i">Twist</p>
                <div id="j"></div>
                <div id="k">
                  <span id="l">Mashed Potato</span>
                </div>
              </div>
              <p id="l">Niagra Falls</p>
            </div>

  def element(tag: String, id: String)(implicit ns: NodeSeq) = ns \\ tag filter (_ \ "@id" contains Text(id))


  test("Child combinator doesn't apply to all descendants") {

    val pSelector = SingleSelector(TypeSelector("p") :: Nil, None)
    val idSelector = SingleSelector(
      IdSelector("a") :: Nil,
      Some((Child, pSelector))
    )
    val selector = Selector(idSelector :: Nil)

    def transformation(elem: Elem): NodeSeq =
      elem % Attribute("class", Text("whack"), Null)

    val transform = selector.transform(transformation)
    implicit val result = Await.result(transform(xml), 1.second)

    (element("p", "l") \ "@class")(0) should equal (Text("whack"))
    (element("p", "d") \ "@class") should  have length (0)
  }

  test("Combinator can apply to multiple children of same parent") {

    val pSelector = SingleSelector(TypeSelector("p") :: Nil, None)
    val idSelector = SingleSelector(
      IdSelector("a") :: Nil,
      Some((Descendant, pSelector))
    )
    val selector = Selector(idSelector :: Nil)

    def transformation(elem: Elem): NodeSeq =
      elem % Attribute("class", Text("thud"), Null)

    val transform = selector.transform(transformation)
    implicit val result = Await.result(transform(xml), 1.second)

    (element("p", "d") \ "@class")(0) should equal (Text("thud"))
    (element("p", "e") \ "@class")(0) should equal (Text("thud"))
    (element("p", "f") \ "@class")(0) should equal (Text("thud"))
  }

  test("Descendant combinator can apply to both a parent its child") {

    val divSelector = SingleSelector(TypeSelector("div") :: Nil, None)
    val idSelector = SingleSelector(
      IdSelector("a") :: Nil,
      Some((Descendant, divSelector))
    )
    val selector = Selector(idSelector :: Nil)

    def transformation(elem: Elem): NodeSeq =
      elem % Attribute("class", Text("smack"), Null)

    val transform = selector.transform(transformation)
    implicit val result = Await.result(transform(xml), 1.second)

    (element("div", "c") \ "@class")(0) should equal (Text("smack"))
    (element("div", "g") \ "@class")(0) should equal (Text("smack"))
  }

  test("Adjacent sibling combinator applies only to the immediate sibling") {

    val pSelector = SingleSelector(TypeSelector("p") :: Nil, None)
    val firstPSelector = pSelector.copy(chain = Some((AdjacentSibling, pSelector)))
    val idSelector = SingleSelector(
      IdSelector("a") :: Nil,
      Some((Descendant, firstPSelector))
    )
    val selector = Selector(idSelector :: Nil)

    def transformation(elem: Elem): NodeSeq =
      elem % Attribute("class", Text("boff"), Null)

    val transform = selector.transform(transformation)
    implicit val result = Await.result(transform(xml), 1.second)

    (element("p", "d") \ "@class") should have length (0)
    (element("p", "e") \ "@class")(0) should equal (Text("boff"))
    (element("p", "f") \ "@class")(0) should equal (Text("boff"))
    (element("p", "i") \ "@class") should have length (0)
  }

  test("General sibling combinator applies to all following siblings") {

    val pSelector = SingleSelector(TypeSelector("p") :: Nil, None)
    val firstPSelector = pSelector.copy(chain = Some((GeneralSibling, pSelector)))
    val idSelector = SingleSelector(
      IdSelector("a") :: Nil,
      Some((Descendant, firstPSelector))
    )
    val selector = Selector(idSelector :: Nil)

    def transformation(elem: Elem): NodeSeq =
      elem % Attribute("class", Text("crash"), Null)

    val transform = selector.transform(transformation)
    implicit val result = Await.result(transform(xml), 1.second)

    (element("p", "d") \ "@class") should have length (0)
    (element("p", "e") \ "@class")(0) should equal (Text("crash"))
    (element("p", "f") \ "@class")(0) should equal (Text("crash"))
    (element("p", "i") \ "@class")(0) should equal (Text("crash"))
  }

  test("General sibling combinator applies only once to each selected element") {

    val pSelector = SingleSelector(TypeSelector("p") :: Nil, None)
    val firstPSelector = pSelector.copy(chain = Some((GeneralSibling, pSelector)))
    val idSelector = SingleSelector(
      IdSelector("a") :: Nil,
      Some((Descendant, firstPSelector))
    )
    val selector = Selector(idSelector :: Nil)

    var count = 0
    def transformation(elem: Elem): NodeSeq = {
      count += 1
      elem
    }

    val transform = selector.transform(transformation)
    implicit val result = Await.result(transform(xml), 1.second)

    count should be (3)
  }
}