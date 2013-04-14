package islay.template

import scala.xml.NodeSeq

import spray.http.ContentType


trait Formatter {
  def contentType: ContentType
  def format(nodes: NodeSeq): Array[Byte]
}