package islay.example.pages

import islay.example.TemplateProcessorModule
import islay.transform._
import islay.web.{Error, FieldError, Message, Page}
import islay.web.Directives
import spray.routing.Route
import scala.xml.PCData
import scala.xml.Unparsed


trait FeedbackSnippetModule {
  dependsOn: TemplateProcessorModule =>

  import Directives._

  def feedbackSnippet: Route = dynamic (
    allMessages (
      new FeedbackSnippet(_)
    )
  )

  class FeedbackSnippet(messages: Map[Symbol, Seq[Message]]) extends Page {

    def script = <script>{Unparsed("""
  var cleaned = window.location.href.replace(/[?&]!flash=[^&]*/, '')
  if (window.history.replaceState && cleaned != window.location.href)
    window.history.replaceState('', '', cleaned)
""")}</script>


    def allErrors = messages.getOrElse('error, Nil)
    def alerts = messages.getOrElse('alert, Nil)
    def notices = messages.getOrElse('notice, Nil)

    def errors = {
      val (fieldErrors, others) = allErrors.partition(_.isInstanceOf[FieldError])
      fieldErrors match {
        case Nil => others
        case _ => new Message(Error.Bundle, "field.errors", fieldErrors.length) +: others
      }
    }

    def byClass = Seq("error" -> errors, "alert" -> alerts, "notice" -> notices)

    override def transform = (
      c"head".prepend(script) &
      c".feedback" <> byClass.flatMap { case (className, ms) =>
        ms.map { m =>
          c"*".addClass(className) &
          c"*" ** m
        }
      }
    )
  }
}
