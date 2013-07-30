package islay.web

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

import WebHeaders._
import akka.actor._
import akka.spray.UnregisteredActorRef
import islay.template.TemplateDirectives
import islay.transform.CallingThreadExecutor
import shapeless._
import spray.http.{ChunkedResponseStart, Confirmed, HttpResponse, Uri}
import spray.routing.{Directive0, Directive1, RequestContext}
import spray.routing.Directive
import spray.routing.directives._


trait WebDirectives {
  import BasicDirectives._
  import MiscDirectives._
  import PathDirectives._
  import RespondWithDirectives._
  import TemplateDirectives._
  import WebHeaders._

  /**
   * Adds a `Refresh` header to the response with a timeout of 0 and the given URI.
   */
  def refresh(url: String): Directive0 = refresh(0, url)

  /**
   * Adds a `Refresh` header to the response with the given timeout and URI.
   */
  def refresh(timeout: Int, url: String): Directive0 = respondWithHeader(Refresh(timeout, url))

  /**
   * Overwrites the path part of the request URI with the given absolute path. This is useful when
   * the request path needs some munging to map to an HTML template.
   */
  def rewritePath(path: String): Directive0 = mapRequestContext { ctx =>
    val absolutePath = if (path startsWith "/") path else "/"+ path
    val uri = ctx.request.uri.copy(path = Uri.Path(absolutePath))
    RequestContext(ctx.request.copy(uri = uri), ctx.responder, uri.path)
  }

  /**
   * Adds the given message to flash scope at ''notice'' level.
   */
  def flash(content: Message): Directive0 = flash('notice, content)

  /**
   * Adds the given message to flash scope at the specified level. Messages in flash scope are
   * visible in the next request following a redirect or refresh response. Messages can be
   * retreived from flash scope using either the `messages` or `flashMessages` directives.
   *
   * Note that unlike normal messages, a message stored in flash scope cannot contain NodeSeqs as
   * format arguments as this would lead to XSS problems under the current RESTful implementation.
   * Markup can be safely contained in strings loaded from the property resource bundle though.
   */
  def flash(level: Symbol, content: Message): Directive0 = {
    import CallingThreadExecutor.Implicit
    mapRequestContext { ctx =>
      withHttpResponseMappedFuture(ctx) { response =>
        WebHeaders.addFlashMessage(response, level, content)
      }
    }
  }

  def withHttpResponseMappedFuture(ctx: RequestContext)(f: HttpResponse => Future[HttpResponse])
      (implicit executor: ExecutionContext) =
    withRouteResponseMappedFuture(ctx) {
      case x: HttpResponse                         ⇒ f(x)
      case ChunkedResponseStart(x)                 ⇒ f(x).map(ChunkedResponseStart(_))
      case Confirmed(ChunkedResponseStart(x), ack) ⇒ f(x).map(y => Confirmed(ChunkedResponseStart(y), ack))
      case Confirmed(x: HttpResponse, ack)         ⇒ f(x).map(Confirmed(_, ack))
      case x                                       ⇒ Future.successful(x)
    }

  private def withRouteResponseMappedFuture(ctx: RequestContext)(f: Any => Future[Any])
      (implicit executor: ExecutionContext) =
    ctx.withResponder {
      new UnregisteredActorRef(ctx.responder) {
        def handle(message: Any)(implicit sender: ActorRef) {
          f(message) onComplete {
            case Success(m) => ctx.responder ! m
            case Failure(x) => ctx.responder ! Status.Failure(x)
          }
        }
      }
    }


  def requestAttr[T](name: String): Directive[Option[T] :: HNil] = ???
  def requestAttr(name: String, value: Any): Directive0 = ???


  /**
   * Adds the given message to request scope at ''error'' level.
   */
  def error(msg: Message): Directive0 = message('error, msg)

  /**
   * Adds the given message to request scope at ''alert'' level.
   */
  def alert(msg: Message): Directive0 = message('alert, msg)

  /**
   * Adds the given message to request scope at ''notice'' level.
   */
  def notice(msg: Message): Directive0 = message('notice, msg)

  /**
   * Adds the given message to request scope at the specified level. Level is an arbitrary marker
   * with whatever semantics you give it.
   */
  def message(level: Symbol, msg: Message): Directive0 = messages(level, List(msg))

  /**
   * Adds the given messages to request scope at the specified level. Level is an arbitrary marker
   * with whatever semantics you give it.
   */
  def messages(level: Symbol, msgs: Seq[Message]): Directive0 = mapRequest { addMessages(_, level, msgs) }

  /**
   * Extracts validation errors for a field identified by the given name (the name attribute on the
   * form element).
   */
  def error(name: String): Directive1[Seq[Message]] = extract { ctx =>
    requestMessages(ctx.request).getOrElse('error, Nil) collect {
      case m: FieldError if m.fieldName == name => m
    }
  }

  /**
   * Extracts all ''error'' level messages from request scope.
   */
  def errors: Directive1[Seq[Message]] = messages('error)

  /**
   * Extracts all ''alert'' level messages from request scope.
   */
  def alerts: Directive1[Seq[Message]] = messages('alert)

  /**
   * Extracts all ''notice'' level messages from request scope.
   */
  def notices: Directive1[Seq[Message]] = messages('notice)

  /**
   * Extracts all messages at the given level from request scope.
   */
  def messages(level: Symbol): Directive1[Seq[Message]] =
    originalRequest map { req =>
      WebHeaders.messages(req).getOrElse(level, Nil)
    }

  /**
   * Extracts all messages, grouped by level, from request scope.
   */
  def allMessages: Directive1[Map[Symbol, Seq[Message]]] =
    originalRequest.map(WebHeaders.messages)
}

object WebDirectives extends WebDirectives