package islay.web

import islay.transform.Renderable
import scala.xml.NodeSeq
import scala.concurrent.Future

object Message {

  val defaultBundle = "messages"
}

class Message(key: String, args: Any*)(bundle: String = Message.defaultBundle) extends Renderable {

  override def toNodeSeq: Future[NodeSeq] = ???
}