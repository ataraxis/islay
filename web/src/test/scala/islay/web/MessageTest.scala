package islay.web

import java.util.{GregorianCalendar, Locale}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.DurationInt
import scala.xml.Text

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class MessageTest extends FunSuite with ShouldMatchers {

  implicit val locales = Seq(new Locale("en"))
  implicit val executor = ExecutionContext.Implicits.global

  test("Integer arguments are formatted according to locale") {
    val f = Message("fingers", 5000).toNodeSeq
    Await.result(f, 2.seconds) should equal (Text("The 5,000 fingers of Dr. T"))
  }

  test("Decimal arguments are formatted according to locale") {
    val f = Message("fingers", 5.1).toNodeSeq
    Await.result(f, 2.seconds) should equal (Text("The 5.1 fingers of Dr. T"))
  }

  test("Date arguments are formatted according to locale") {
    val cal = new GregorianCalendar
    cal.set(2012, 7, 31)
    val f = Message("oslo", cal.getTime).toNodeSeq
    Await.result(f, 2.seconds) should equal (Text("Oslo, August 31"))
  }

  test("String arguments are properly escaped") {
    val f = Message("ogre", "<script>").toNodeSeq
    Await.result(f, 2.seconds).toString should equal ("&lt;script&gt; was an ogre.")
  }

  test("Missing message warnings are properly escaped") {
    val f = Message("orge", "<script>").toNodeSeq
    Await.result(f, 2.seconds).toString should equal ("???orge???(&lt;script&gt;)")
  }

  test("NodeSeq arguments are not escaped") {
    val f = Message("ogre", <br/>).toNodeSeq
    Await.result(f, 2.seconds).toString should equal ("<br/> was an ogre.")
  }

  test("Message arguments to other messages are formatted correctly") {
    val f = Message("ogre", Message("greeting")).toNodeSeq
    Await.result(f, 2.seconds).toString should equal ("Hello was an ogre.")
  }
}