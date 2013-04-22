package islay.template

import java.io.{ByteArrayOutputStream, OutputStreamWriter}

import scala.xml.{Elem, NodeSeq, XML}
import scala.xml.dtd.{DocType, SystemID}

import spray.http.{ContentType, HttpCharsets, MediaTypes}


class Html5Formatter extends Formatter {

  override def contentType: ContentType = ContentType(MediaTypes.`text/html`, HttpCharsets.`UTF-8`)

  override def format(nodes: NodeSeq): Array[Byte] = {

    val out = new ByteArrayOutputStream
    val root = nodes.find(_.isInstanceOf[Elem]) getOrElse <html/>
    val docType = DocType("html", SystemID("about:legacy-compat"), Nil)

    XML.write(new OutputStreamWriter(out), root, "UTF-8", false, docType)
    out.toByteArray
  }
}