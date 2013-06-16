package islay.web

import spray.http.{BodyPart, EmptyEntity, FormData, HttpBody, HttpForm, HttpMethods, HttpRequest, MultipartFormData}


object SubmittedValue {

  def extractAll(request: HttpRequest): Map[String, SubmittedValue] = request.entity match {
    case EmptyEntity if request.method == HttpMethods.GET =>
      request.uri.query.map { case (name, value) => (name, TextValue(value)) }.toMap
    case e: HttpBody =>
      import spray.httpx.unmarshalling._
      e.as[HttpForm] match {
        case Right(form) => SubmittedValue.extractAll(form)
        case Left(_) => Map.empty
      }
    case _ =>
      Map.empty
  }

  def extractAll(httpForm: HttpForm): Map[String, SubmittedValue] = httpForm match {
    case FormData(fields) =>
      fields map { case (name, value) =>
        (name, TextValue(value))
      }
    case MultipartFormData(fields) =>
      fields map { case (name, bodyPart) =>
        import spray.httpx.unmarshalling._
        bodyPart.as[String] match {
          case Right(value) => (name, TextValue(value))
          case Left(_) => (name, BodyPartValue(bodyPart))
        }
      }
  }
}

sealed trait SubmittedValue {
  type Type
  def value: Type
  def asString: String
}

case class TextValue(value: String) extends SubmittedValue {
  type Type = String
  def asString = value
}

case class BodyPartValue(value: BodyPart) extends SubmittedValue {
  type Type = BodyPart
  def asString = ""
}