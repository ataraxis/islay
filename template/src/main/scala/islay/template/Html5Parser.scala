package islay.template

import java.io.ByteArrayInputStream

import scala.xml.{Comment, Elem, NodeSeq}
import scala.xml.parsing.NoBindingFactoryAdapter

import org.xml.sax.InputSource
import org.xml.sax.ext.LexicalHandler

import nu.validator.htmlparser.common.XmlViolationPolicy
import nu.validator.htmlparser.sax.HtmlParser


class Html5Parser extends Parser {

  override def parse(bytes: Array[Byte]): NodeSeq = {

    val parser = new HtmlParser(XmlViolationPolicy.ALLOW)
    val handler = new ContentHandler
    parser.setContentHandler(handler)
    parser.setLexicalHandler(handler)

    val input = new InputSource(new ByteArrayInputStream(bytes))
    input.setEncoding("UTF-8")
    parser.parse(input)

    handler.rootElem
  }

  /**
   * `NoBindingFactoryAdapter` with comment handling.
   */
  class ContentHandler extends NoBindingFactoryAdapter with LexicalHandler {

    override def comment(ch: Array[Char], start: Int, length: Int) {
      captureText()
      hStack.push(Comment(ch.mkString))
    }

    def endCDATA() {}
    def endDTD() {}
    def endEntity(name: String) {}
    def startCDATA() {}
    def startDTD(name: String, publicId: String, systemId: String) {}
    def startEntity(name: String) {}
  }

  override def parseFragment(bytes: Array[Byte]): NodeSeq = {
    val nodes = parse(bytes)
    nodes.headOption match {
      case Some(html: Elem) if html.label == "html" =>
        html.child.lastOption match {
          case Some(body: Elem) if body.label == "body" =>
            body.child
          case _ => nodes
        }
      case _ => nodes
    }
  }
}