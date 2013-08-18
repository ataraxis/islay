package islay.web

import java.nio.file.NoSuchFileException
import java.util.Locale

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.xml.NodeSeq

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import WebHeaders._
import spray.http.{HttpRequest, HttpResponse}
import spray.http.HttpHeaders.Location


class WebHeadersTest extends FunSuite with ShouldMatchers {

  val response = HttpResponse(headers = Location("somewhere") :: Nil)
  implicit val locales = Seq(new Locale("en"))
  implicit val executor = ExecutionContext.global


  def encodeAndDecode(message: Message): NodeSeq = {
    val fut = addFlashMessage(response, 'notice, message)
    val resp = Await.result(fut, 3.seconds)
    val url = resp.headers.collectFirst { case Location(url) => url }.get

    val req = HttpRequest(uri = url)
    val Seq(decoded) = flashMessages(req)('notice)
    Await.result(decoded.toNodeSeq, 3.seconds)
  }

  test("Encoded string arguments are decoded to their original values") {
    val message = Message("user.saved", Some("Hannibal"))
    val text = encodeAndDecode(message).toString
    text should be ("User <cite>Hannibal</cite> was saved successfully.")
  }

  test("Encoded number arguments are decoded to their original values") {
    val message = Message("cart.total", Future.successful(1.99), 2)
    val text = encodeAndDecode(message).toString
    text should be ("$1.99 (2 items)")
  }

  test("Twiddle and backslash are properly escaped and unescaped") {
    val message = Message("user.saved", "E\\~rrol~")
    val text = encodeAndDecode(message).toString
    text should be ("User <cite>E\\~rrol~</cite> was saved successfully.")
  }

  test("NodeSeq arguments are escaped") {
    val message = Message("user.saved", <script>alert(1)</script>)
    val text = encodeAndDecode(message).toString
    text should be ("User <cite>&amp;lt;script&amp;gt;alert(1)&amp;lt;/script&amp;gt;</cite> was saved successfully.")
  }

  test("Path traversal is not possible on bundle name") {
    val url = "somewhere?!flash=notice~../properties/messages~user.saved~:Edwin"
    val req = HttpRequest(uri = url)
    val Seq(decoded) = flashMessages(req)('notice)
    intercept[NoSuchFileException] {
      Await.result(decoded.toNodeSeq, 3.seconds)
    }
  }
}