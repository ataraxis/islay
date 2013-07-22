package islay.web.components

import scala.util.{Failure, Success, Try}
import scala.xml.Text

import islay.web.{BodyPartValue, Error, SubmittedValue, TextValue}
import spray.http.{EmptyEntity, HttpBody}


object FieldConverter {

  type Result[V] = Either[Error, V]

  def single[V](f: => V, message: String): Result[V] = Try(f) match {
    case Success(v) => Right(v)
    case Failure(ex) => Left(Error(Text(message)))
  }

  implicit object StringConverter extends FieldConverter[String] {
    override def name = "string"
    override def fromString(value: String): Result[String] = Right(value)
  }

  implicit object IntConverter extends FieldConverter[Int] {
    override def name = "integer"
    override def fromString(value: String): Result[Int] =
      if (value.isEmpty) Right(0)
      else single(value.toInt, "Invalid integer")
  }

  implicit object DoubleConverter extends FieldConverter[Double] {
    override def name = "decimal number"
    override def fromString(value: String): Result[Double] =
      if (value.isEmpty) Right(0.0)
      else single(value.toDouble, "Invalid decimal number")
  }

  implicit def optionConverter[A](implicit delegate: FieldConverter[A]): FieldConverter[Option[A]] =
    new FieldConverter[Option[A]] {
      override def toString(value: Option[A]): String = value.fold("")(delegate.toString)
      override def fromString(value: String): Result[Option[A]] =
        if (value.isEmpty) Right(None)
        else delegate.fromString(value).right.map(Some(_))
    }
}


abstract class FieldConverter[A] {
  import FieldConverter._

  def toString(value: A): String = value.toString

  def fromSubmitted(value: SubmittedValue): Result[A] =
    value match {
      case TextValue(text) => fromString(text)
      case BodyPartValue(bodyPart) => bodyPart.entity match {
        case HttpBody(contentType, _) =>
          Left(Error(Text("Cannot extract "+ name +" from `"+ contentType.value +"`")))
        case EmptyEntity =>
          Left(Error(Text("Cannot extract "+ name +" from empty entity")))
      }
  }

  protected def fromString(value: String): Result[A] = ???
  protected def name: String = ???
}