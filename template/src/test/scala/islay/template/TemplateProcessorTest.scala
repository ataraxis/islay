package islay.template

import java.io.FileNotFoundException

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.xml.{Comment, NodeSeq}

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import akka.actor._
import akka.testkit.{ImplicitSender, TestKit}
import spray.http.HttpRequest
import spray.routing.RequestContext
import spray.routing.directives.{PathDirectives, RouteDirectives}


class TemplateProcessorTest extends TestKit(ActorSystem("test")) with ImplicitSender
with FunSuite with ShouldMatchers
with RouteDirectives with PathDirectives with TemplateDirectives {

  def stubParser(content: NodeSeq) = new Parser {
    override def parse(bytes: Array[Byte]) = content
  }

  val processor = new TemplateProcessor(
    route = completeTemplate(<buffalo/>),
    root = Resources.pathTo("webapp"),
    formatter = null,
    parsers = Map("html" -> stubParser(<html/>))
  )

  test("A resource with a known file extension can be found at an exact path") {
    val request = HttpRequest(uri = "index.html").parseUri
    val result = Await.result(processor.lookup(request), 1.second)
    result should equal (<html/>)
  }

  test("A resource can be found by appending an available file extension") {
    val request = HttpRequest(uri = "/index").parseUri
    val result = Await.result(processor.lookup(request), 1.second)
    result should equal (<html/>)
  }

  test("A default resource named 'index' can be found at a directory path") {
    val request = HttpRequest(uri = "/").parseUri
    val result = Await.result(processor.lookup(request), 1.second)
    result should equal (<html/>)
  }

  test("A `FileNotFoundException` failure is returned for a path that cannot be matched") {
    val request = HttpRequest(uri = "plah").parseUri
    intercept[FileNotFoundException] {
      Await.result(processor.lookup(request), 1.second)
    }
  }

  test("SSI expansion uses include's path to route") {
    val route = path("buffalo" / IntNumber) { i =>
      i should be (1)
      completeTemplate(<buffalo/>)
    }
    val ssi = Comment("""#include file="/buffalo/1"""")
    val parser = Map("html" -> stubParser(ssi))
    val p = processor.copy(route = route, parsers = parser)

    val context = RequestContext(request = HttpRequest(), responder = self)

    p.expand(ssi, context) match {
      case None => fail("No expansion result")
      case Some(f) =>
        val result = Await.result(f, 1.second)
        result.toString should equal ("<buffalo/>")
    }
  }

  test("SSI expansion occurs within a nested node structure") {
    val ssi = Comment("""#include file="/buffalo"""")
    val parser = Map("html" -> stubParser(ssi))
    val p = processor.copy(parsers = parser)

    val context = RequestContext(request = HttpRequest(), responder = self)

    val nodes = <p/><div>{Comment("Pop")} {ssi}</div><br/>;
    p.expand(nodes, context) match {
      case None => fail("No expansion result")
      case Some(f) =>
        val result = Await.result(f, 1.second)
        result.toString should equal ("<p/><div><!--Pop--> <buffalo/></div><br/>")
    }
  }
}