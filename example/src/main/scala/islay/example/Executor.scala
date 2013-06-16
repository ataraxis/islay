package islay.example

import scala.concurrent.ExecutionContext

import spray.routing.HttpService


trait ExecutorModule extends HttpService {

  implicit def executor: ExecutionContext
}