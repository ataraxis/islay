package islay.template

import java.io.{ByteArrayOutputStream, OutputStreamWriter}

import scala.xml.{Elem, NodeSeq, XML}
import scala.xml.dtd.{DocType, SystemID}

import spray.http.{ContentType, HttpCharsets, MediaTypes}


class Html5Formatter extends Formatter {

  override def contentType: ContentType = ContentType(MediaTypes.`text/html`, HttpCharsets.`UTF-8`)

  override def format(result: TemplateResult): Array[Byte] = {

    val out = new ByteArrayOutputStream

    val systemId = new SystemID("") { override def toString = "" }
    val docType = DocType("html", systemId, Nil)

    val writer = new OutputStreamWriter(out)
    XML.write(writer, result.html, "UTF-8", false, docType)
    writer.flush()
    out.toByteArray
  }
}