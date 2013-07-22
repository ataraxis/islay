package islay.web.components

import java.util.Locale

import scala.concurrent.ExecutionContext

import islay.web.{Bundle, Message}


object FieldName {
  implicit object Bundle extends Bundle[FieldName]("fields")
}


/**
 * An internationalized field label. This is used to provide access to the field name from field
 * validation error messages.
 */
case class FieldName(key: String)(implicit bundle: Bundle[FieldName], locales: Seq[Locale], executor: ExecutionContext)
extends Message(bundle, key)