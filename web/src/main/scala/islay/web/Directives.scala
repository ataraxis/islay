package islay.web

import islay.template.TemplateDirectives
import spray.routing.directives.RouteDirectives
import spray.routing.directives.FutureDirectives
import spray.routing.directives.DebuggingDirectives
import spray.routing.directives.PathDirectives
import spray.routing.directives.HostDirectives
import spray.routing.directives.RespondWithDirectives
import spray.routing.directives.FileAndResourceDirectives
import spray.routing.directives.ExecutionDirectives
import spray.routing.directives.MarshallingDirectives
import spray.routing.directives.MiscDirectives
import spray.routing.directives.HeaderDirectives
import spray.routing.directives.ChunkingDirectives
import spray.routing.directives.BasicDirectives
import spray.routing.directives.FormFieldDirectives
import spray.routing.directives.EncodingDirectives
import spray.routing.directives.ParameterDirectives
import spray.routing.directives.MethodDirectives
import spray.routing.directives.CookieDirectives
import spray.routing.directives.AnyParamDirectives
import spray.routing.RouteConcatenation


trait Directives extends WebDirectives with WebRoutes
  with AuthenticationDirectives
  with TemplateDirectives
  with RouteConcatenation
  with AnyParamDirectives
  with BasicDirectives
  with ChunkingDirectives
  with CookieDirectives
  with DebuggingDirectives
  with EncodingDirectives
  with ExecutionDirectives
  with FormFieldDirectives
  with FutureDirectives
  with HeaderDirectives
  with HostDirectives
  with MarshallingDirectives
  with MethodDirectives
  with MiscDirectives
  with ParameterDirectives
  with PathDirectives
  with RespondWithDirectives
  with RouteDirectives

object Directives extends Directives