package islay.web.components

import scala.util.{Failure, Success, Try}

import islay.web.{BodyPartValue, SubmittedValue, TextValue}
import spray.http.{EmptyEntity, HttpBody}


object FieldConverter {

  implicit object StringConverter extends FieldConverter[String] {
    override def name = "string"
    override def fromString(value: String): Try[String] = Success(value)
  }

  implicit object IntConverter extends FieldConverter[Int] {
    override def name = "integer"
    override def fromString(value: String): Try[Int] =
      if (value.isEmpty) Success(0)
      else Try(value.toInt) recover { case _ => sys.error("Invalid integer") }
  }

  implicit object DoubleConverter extends FieldConverter[Double] {
    override def name = "decimal number"
    override def fromString(value: String): Try[Double] =
      if (value.isEmpty) Success(0.0)
      else Try(value.toDouble) recover { case _ => sys.error("Invalid decimal number") }
  }

  implicit def optionConverter[A](implicit delegate: FieldConverter[A]): FieldConverter[Option[A]] =
    new FieldConverter[Option[A]] {
      override def toString(value: Option[A]): String = value.fold("")(delegate.toString)
      override def fromString(value: String): Try[Option[A]] =
        if (value.isEmpty) Success(None)
        else delegate.fromString(value).map(Some(_))
    }
}


abstract class FieldConverter[A] {

  def toString(value: A): String = value.toString

  def fromSubmitted(value: SubmittedValue): Try[A] =
    value match {
      case TextValue(text) => fromString(text)
      case BodyPartValue(bodyPart) => bodyPart.entity match {
        case HttpBody(contentType, _) =>
          Failure(new Exception("Cannot extract "+ name +" from `"+ contentType.value +"`"))
        case EmptyEntity =>
          Failure(new Exception("Cannot extract "+ name +" from empty entity"))
      }
  }

  protected def fromString(value: String): Try[A] = ???
  protected def name: String = ???
}