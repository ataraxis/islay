package islay.template

import java.io.FileNotFoundException

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.xml.{Comment, NodeSeq}

import org.scalatest.{Finders, FunSuite}
import org.scalatest.matchers.ShouldMatchers

import akka.actor._
import akka.testkit.{ImplicitSender, TestKit}
import spray.http.HttpRequest
import spray.routing.Directive._
import spray.routing._
import spray.routing.directives._


class TemplateProcessorTest extends TestKit(ActorSystem("test")) with ImplicitSender
with FunSuite with ShouldMatchers
with RouteDirectives with PathDirectives with TemplateDirectives {

  def stubParser(content: NodeSeq) = new Parser {
    override def parse(bytes: Array[Byte]) = content
    override val contentBinding = "<islay:binding/>".getBytes
  }

  def newProcessor = new TemplateProcessor {
    override val parsers = Map("html" -> stubParser(<html/>))
    override val route = complete(TemplateResult(body = <buffalo/>))
  }

  test("A resource with a known file extension can be found at an exact path") {
    val request = HttpRequest(uri = "index.html").parseUri
    val processor = newProcessor
    val result = Await.result(processor.lookup(request), 1.second)
    result should equal (<html/>)
  }

  test("A resource can be found by appending an available file extension") {
    val request = HttpRequest(uri = "/index").parseUri
    val processor = newProcessor
    val result = Await.result(processor.lookup(request), 1.second)
    result should equal (<html/>)
  }

  test("A default resource named 'index' can be found at a directory path") {
    val request = HttpRequest(uri = "/").parseUri
    val processor = newProcessor
    val result = Await.result(processor.lookup(request), 1.second)
    result should equal (<html/>)
  }

  test("A `FileNotFoundException` failure is returned for a path that cannot be matched") {
    intercept[FileNotFoundException] {
      val request = HttpRequest(uri = "plah").parseUri
      val processor = newProcessor
      Await.result(processor.lookup(request), 1.second)
    }
  }

  test("SSI expansion uses include's path to route") {

    val ssi = Comment("""#include file="/buffalo/1"""")
    val parser = Map("html" -> stubParser(ssi))

    implicit val processor = new TemplateProcessor(parsers = parser) {
      override val route = path("buffalo" / IntNumber) { i =>
        i should be (1)
        complete(TemplateResult(body = <buffalo/>))
      }
    }

    val context = RequestContext(request = HttpRequest(), responder = self)

    val f = processor.expand(ssi, context)
    val result = Await.result(f, 1.second)
    result.body.toString should equal ("<buffalo/>")
  }

  test("SSI expansion occurs within a nested node structure") {

    val ssi = Comment("""#include file="/buffalo"""")
    val processor = newProcessor
    val context = RequestContext(request = HttpRequest(), responder = self)

    val nodes = <p/><div>{Comment("Pop")} {ssi}</div><br/>;
    val f = processor.expand(nodes, context)
    val result = Await.result(f, 1.second)
    result.body.toString should equal ("<p/><div><!--Pop--> <buffalo/></div><br/>")
  }

  test("Header and footer SSI expansion includes surrounded content") {

    val processor = new TemplateProcessor {
      override val route: Route =
        path("header.html") { ctx =>
          ctx.request.headers collectFirst { case SurroundEnd(p) => p } should be (Some("/footer.html?!"))
          complete(TemplateResult(body = <header/><islay:binding/><footer/>), ctx)
        }
    }

    val header = Comment("""#include file="/header.html"""")
    val footer = Comment("""#include file="/footer.html?!"""")
    val nodes = <div>{header}surrounded{footer}</div>

    val context = RequestContext(request = HttpRequest(), responder = self)

    val f = processor.expand(nodes, context)
    val result = Await.result(f, 1.second)
    result.body.toString should equal ("<div><header/>surrounded<footer/></div>")
  }

  test("SSI lookup includes bytes from both header and footer") {

    var text = ""
    val parser = new Parser {
      override def parse(bytes: Array[Byte]) = {
        text += new String(bytes)
        <crash/>
      }
      override def contentBinding = "<boff/>".getBytes
    }

    val processor = newProcessor.copy(parsers = Map("html" -> parser))
    val request = HttpRequest(uri = "/header.html", headers = List(SurroundEnd("/footer.html?!"))).parseUri

    val nodes = Await.result(processor.lookup(request), 1.second)
    text should equal ("<div><h1>Header</h1><boff/>!</div>")
    nodes.toString should equal ("<crash/>")
  }

  test("Head is merged from body") {

    val nodes =
      <html><head><title>Buffalo</title></head><body><head><title>Boff</title></head>Boofalu?</body></html>

    val processor = newProcessor
    val context = RequestContext(request = HttpRequest(), responder = self)
    processor.expand(nodes, context)

    val f = processor.expand(nodes, context)
    val result = Await.result(f, 1.second)
    result.head.toString should equal ("<title>Buffalo</title><title>Boff</title>")
    result.body.toString should equal ("<html><body>Boofalu?</body></html>")
  }
}