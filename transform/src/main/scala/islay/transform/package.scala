package islay

import scala.language.experimental.macros

import scala.concurrent.Future
import scala.xml.{Elem, NodeSeq}

import islay.transform.{Selector, SelectorImpl}


package object transform {

  type Transformation = Elem => Future[NodeSeq]


  implicit class SelectorContext(val sc: StringContext) {

    def c(): Selector = macro SelectorImpl.cImpl

    def c(args: Any*): Selector = {
      new Selector(Nil)
    }
  }
}