package islay.web.components

import islay.transform.{Bindable, Selector, Transform}
import islay.transform.parser.{AttributeOperator, AttributeSelector, SingleSelector}
import islay.web.SubmittedValue


trait Component extends Bindable {

  def transform: Transform = selector <> Component.this

  def selector: Selector = {
    val attributeSelector = AttributeSelector("name", Some((AttributeOperator.ExactValue, name)))
    Selector(Seq(SingleSelector(Seq(attributeSelector), None)))
  }

  def name: String
  private[islay] def complete(name: String, value: Option[SubmittedValue]): Unit
}