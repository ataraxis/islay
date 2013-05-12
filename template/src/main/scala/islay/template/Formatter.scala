package islay.template

import spray.http.ContentType


trait Formatter {
  def contentType: ContentType
  def format(result: TemplateResult): Array[Byte]
}