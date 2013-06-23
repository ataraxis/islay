package islay.template

import java.io.ByteArrayInputStream

import scala.xml._
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

    val bytesWithMarker = Array.concat(bytes, "<!--islay-marker-->".getBytes)
    val input = new InputSource(new ByteArrayInputStream(bytesWithMarker))
    input.setEncoding("UTF-8")
    parser.parse(input)

    handler.result
  }

  override val contentBinding: Array[Byte] = "<islay:binding/>".getBytes

  /**
   * `NoBindingFactoryAdapter` with comment handling.
   */
  class ContentHandler extends NoBindingFactoryAdapter with LexicalHandler {

    override def comment(ch: Array[Char], start: Int, length: Int) {
      captureText()
      hStack.push(Comment(ch.mkString))
    }


    /* only minimize void tags */
    override def create(pre: String, label: String, attrs: MetaData, scope: NamespaceBinding, children: Seq[Node]): Elem =
       Elem(pre, label, attrs, scope, voidTags.contains(label), children: _*)

    /* only minimize void tags */
    override def createNode(pre: String, label: String, attrs: MetaData, scope: NamespaceBinding, children: List[Node]): Elem =
      Elem(pre, label, attrs, scope, voidTags.contains(label), children: _*)

    private val voidTags = Set(
      "area", "base", "br", "col", "command", "embed", "hr", "img", "input",
      "keygen", "link", "meta", "param", "source", "track", "wbr"
    )

    /**
     * The parsed document stripped of auto-inserted html, head and body tags. By the location of
     * `<!--islay-marker-->` we can tell whether an html or body tag was inserted. Let's handle
     * these cases:
     *
     * - html and body added:
     *  - `Stack(<html><head><title>Foo</title></head><body>Hello<!--islay-marker--></body></html>)`
     * - html added:
     *  - `Stack(<html><head><title>Foo</title></head><body>Hello</body><!--islay-marker--></html>)`
     * - nothing added:
     *  - `Stack(<html><head><title>Foo</title></head><body>Hello</body></html>, <!--islay-marker-->)`
     *
     */
    def result: NodeSeq = {

      /* no sure way to check if head or body was auto-inserted */
      def autoInserted(label: String, node: Node) =
        node.label == label && node.child.isEmpty && node.attributes == Null

      def userInserted(label: String, node: Node) =
        node.label == label && !autoInserted(label, node)


      val nodes = hStack.reverse
      nodes.last match {
        case Comment("islay-marker") =>
          nodes.dropRight(1)
        case _ =>
          nodes flatMap {
            case html: Elem if html.label == "html" =>
              html.child collectFirst {
                case Comment("islay-marker") =>
                  html.child filter { n => userInserted("head", n) || userInserted("body", n) }
              } getOrElse {
                html.child collectFirst {
                  case body: Elem if body.label == "body" =>
                    body.child.lastOption match {
                      case Some(Comment("islay-marker")) =>
                        html.child.filter(userInserted("head", _)) ++ body.child.dropRight(1)
                      case _ =>
                        sys.error("Couldn't find islay-marker in auto-inserted markup")
                    }
                } getOrElse sys.error("Missing auto-inserted body element")
              }
            case n => n
          }
      }
    }

    def endCDATA() {}
    def endDTD() {}
    def endEntity(name: String) {}
    def startCDATA() {}
    def startDTD(name: String, publicId: String, systemId: String) {}
    def startEntity(name: String) {}
  }
}