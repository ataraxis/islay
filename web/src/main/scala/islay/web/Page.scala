package islay.web

import java.util.Locale

import islay.template.{TemplateBinding, TemplateProcessor}
import spray.http.HttpHeaders.`Accept-Language`


abstract class Page(override implicit val processor: TemplateProcessor) extends TemplateBinding {

  implicit val context: WebContext = WebContext.from(request)
  implicit val locales: Seq[Locale] = context.locales
}