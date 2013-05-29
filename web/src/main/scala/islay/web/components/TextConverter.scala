package islay.web.components

import scala.util.{Success, Try}


/**
 * Defines implicit TextConverter parameters.
 */
object TextConverter {

  implicit object StringTextConverter extends TextConverter[String] {
    override def fromString(value: String): Try[String] = Success(value)
  }

  implicit object IntTextConverter extends TextConverter[Int] {
    override def fromString(value: String): Try[Int] =
      if (value.isEmpty) Success(0)
      else Try(value.toInt) recover { case _ => sys.error("Invalid integer") }
  }

  implicit object DoubleTextConverter extends TextConverter[Double] {
    override def fromString(value: String): Try[Double] =
      if (value.isEmpty) Success(0.0)
      else Try(value.toDouble) recover { case _ => sys.error("Invalid decimal number") }
  }

  implicit def optionTextConverter[A](implicit delegate: TextConverter[A]): TextConverter[Option[A]] =
    new TextConverter[Option[A]] {
      override def fromString(value: String): Try[Option[A]] =
        if (value.isEmpty) Success(None)
        else delegate.fromString(value).map(Some(_))
    }
}


abstract class TextConverter[A] {
  def toString(value: A): String = value.toString
  def fromString(value: String): Try[A]
}