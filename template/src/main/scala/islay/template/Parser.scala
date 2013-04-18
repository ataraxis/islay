package islay.template

import scala.xml.NodeSeq


trait Parser {
  def parse(bytes: Array[Byte]): NodeSeq
  def parseFragment(bytes: Array[Byte]): NodeSeq
}