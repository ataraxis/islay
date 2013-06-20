package islay.web

import spray.routing.Route
import islay.template.TemplateSettings
import islay.template.TemplateProcessor
import islay.template.util.Resources
import islay.transform.Transform
import spray.routing.RouteConcatenation
import spray.routing.directives._
import islay.template.util.ResourceCache
import spray.http.StatusCodes
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import spray.http.HttpResponse
import spray.http.HttpEntity
import spray.http.MediaTypes
import spray.http.MediaType
import spray.http.ContentType
import spray.http.HttpHeaders
import spray.http.DateTime
import java.nio.file.Files
import scala.util.Success
import scala.util.Failure
import spray.http.HttpMethods
import java.util.NoSuchElementException
import java.nio.file.NoSuchFileException
import org.webjars.WebJarAssetLocator


trait WebRoutes {
  import MethodDirectives._
  import PathDirectives._
  import RouteConcatenation._


  def defaultRoute(implicit executor: ExecutionContext): Route = webjars ~ webResources


  def page(t: Transform)(implicit processor: TemplateProcessor): Route =
    new Page { override def transform = t }

  /**
   * A route that resolves webjar assets from request paths of the form `/webjars/<asset name>`
   * (eg, `/webjars/jquery.min.js`).
   */
  def webjars(implicit executor: ExecutionContext): Route =
    pathPrefix("webjars") { ctx =>
      Future {
        val path = ctx.unmatchedPath.toString.stripPrefix("/")
        val locator = new WebJarAssetLocator
        locator.getFullPath(path)
      } onComplete {
        case Success(fullPath) => classpathResource(fullPath).apply(ctx)
        case Failure(ex: IllegalArgumentException) => println(ex); ctx.reject()
        case Failure(ex) => ctx.failWith(ex)
      }
    }

  /**
   * A route that resolves static resources under the path prefixes specified by the setting
   * `islay.template.static-paths`.
   */
  def webResources(implicit executor: ExecutionContext): Route = {
    val pass: Route = _.reject()
    val routes = TemplateSettings.staticPaths.map(webResources)
    routes.foldLeft(pass)(_ ~ _)
  }

  /**
   * A route that resolves request paths beginning with `prefix` to a static resource under the
   * webapp dir. Paths that don't begin with `prefix` will be rejected, otherwise resources that
   * can't be located will be completed with an immediate 404.
   */
  def webResources(prefix: String)(implicit executor: ExecutionContext): Route =
    classpathResources("webapp", prefix)

  /**
   * A route that resolves request paths beginning with `prefix` to a static resource on the
   * classpath under `resourceDir`. Paths that don't begin with `prefix` will be rejected,
   * otherwise resources that can't be located will be completed with an immediate 404.
   */
  def classpathResources(resourceDir: String, prefix: String)(implicit executor: ExecutionContext): Route =
    pathPrefix(prefix) { ctx =>
      val resourcePath = resourceDir +"/"+ ctx.request.uri.path
      classpathResource(resourcePath).apply(ctx)
    }

  /**
   * A route that completes with a static resource on the classpath. If the resource does not exist
   * a 404 response will be returned; if the method is not GET then a 405 response will be
   * returned.
   */
  def classpathResource(path: String)
      (implicit executor: ExecutionContext): Route = { ctx =>
    val response = for {
      resource <- Future(Resources.pathTo(path))
      if ctx.request.method == HttpMethods.GET
      lastModified <- Future(Files.getLastModifiedTime(resource).toMillis)
      bytes <- Resources.readAllBytes(resource)
      lastModifiedDateTime = DateTime(math.min(lastModified, System.currentTimeMillis))
      lastModifiedHeader = HttpHeaders.`Last-Modified`(lastModifiedDateTime)
      contentType = ContentTypeResolver.Default(path)
    } yield HttpResponse(StatusCodes.OK, HttpEntity(contentType, bytes), lastModifiedHeader :: Nil)
    response onComplete {
      case Success(response) => ctx.complete(response)
      case Failure(error: NoSuchFileException) => ctx.complete(StatusCodes.NotFound)
      case Failure(error: NoSuchElementException) => ctx.complete(StatusCodes.MethodNotAllowed)
      case Failure(error) => ctx.failWith(error)
    }
  }
}

object WebRoutes extends WebRoutes