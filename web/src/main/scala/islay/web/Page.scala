package islay.web

import islay.template.{TemplateBinding, TemplateProcessor}


abstract class Page(override implicit val processor: TemplateProcessor) extends TemplateBinding