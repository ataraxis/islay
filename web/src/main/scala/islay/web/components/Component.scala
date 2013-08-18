package islay.web.components

import islay.transform.{Bindable, Selector, Transform}
import islay.transform.parser.{AttributeOperator, AttributeSelector, SingleSelector}
import islay.web.SubmittedValue


trait Component extends Bindable {

  /**
   * A DOM transformation that binds this component to a form field element with a matching name
   * attribute.
   */
  def transform: Transform = selector <> Component.this

  /**
   * A CSS selector for form elements with a name attribute matching this component.
   */
  def selector: Selector = {
    val attributeSelector = AttributeSelector("name", Some((AttributeOperator.ExactValue, name)))
    Selector(Seq(SingleSelector(Seq(attributeSelector), None)))
  }

  /**
   * Returns the form parameter name for this field.
   */
  def name: String

  private[islay] def complete(name: String, value: Option[SubmittedValue]): Unit
}