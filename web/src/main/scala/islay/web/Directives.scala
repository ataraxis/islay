package islay.web

import islay.template.TemplateDirectives


trait Directives extends WebDirectives with TemplateDirectives with spray.routing.Directives

object Directives extends Directives