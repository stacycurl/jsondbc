package jsondbc.util

import jsondbc.className
import org.scalatest.FreeSpecLike

import scala.reflect.ClassTag


trait JSpec extends FreeSpecLike {
  def assertThrows[T <: Throwable: ClassTag](expectedMessage: String)(f: ⇒ Any): Unit = {
    val message = getMessage[T](f).getOrElse(sys.error("Expected exception: " + className[T]))

    assert(message === expectedMessage)
  }

  private def getMessage[T <: Throwable: ClassTag](f: ⇒ Unit): Option[String] = getThrowable[T](f).map(_.getMessage)

  private def getThrowable[T <: Throwable: ClassTag](f: ⇒ Unit): Option[T] = try { f; None } catch {
    case t: Throwable ⇒ Some(castTo[Throwable, T](t).getOrElse(sys.error(s"Invalid exception, expected ${className[T]}, got: $t")))
  }

  private def castTo[A, B](self: A)(implicit tag: ClassTag[B]): Option[B] =
    if (tag.runtimeClass.isAssignableFrom(self.getClass)) Some(self.asInstanceOf[B]) else None
}
