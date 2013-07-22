package islay.example.pages

import islay.example.TemplateProcessorModule
import islay.transform._
import islay.web.{Error, FieldError, Message, Page}
import islay.web.Directives
import spray.routing.Route


trait FeedbackSnippetModule {
  dependsOn: TemplateProcessorModule =>

  import Directives._

  def feedbackSnippet: Route = dynamic (
    messages() (
      new FeedbackSnippet(_)
    )
  )

  class FeedbackSnippet(messages: Map[Symbol, Seq[Message]]) extends Page {

    def allErrors = messages.getOrElse('error, Nil)
    def warnings = messages.getOrElse('warning, Nil)
    def notices = messages.getOrElse('notice, Nil)

    def errors = {
      val (fieldErrors, others) = allErrors.partition(_.isInstanceOf[FieldError])
      fieldErrors match {
        case Nil => others
        case seq => new Message(Error.Bundle, "field.errors", seq.length) +: others
      }
    }

    override def transform = (
      c".feedback" <> errors.map ( e =>
        c"*".addClass("error") &
        c"*" ** e
      )
    )
  }
}
