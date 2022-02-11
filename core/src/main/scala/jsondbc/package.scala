import jsondbc.util.Extractor
import org.reflections.Reflections

import scala.collection.JavaConverters.asScalaSetConverter
import scala.collection.immutable.List
import scala.reflect.ClassTag
import scala.util.Try
import scala.util.matching.Regex

package object jsondbc {
  implicit class RegexMatcher(private val self: StringContext) extends AnyVal {
    def r: Regex = self.parts.mkString("(.+)")
      .replaceAllLiterally("?", "\\?")
      .r
  }

  implicit class ClassSyntax[A](private val self: Class[A]) {
    def implementationsIn(prefix: String): List[Class[_ <: A]] =
      new Reflections(prefix).getSubTypesOf(self).asScala.toList
  }

  def className[A: ClassTag]: String =
    klassOf[A].getName

  def klassOf[A](implicit tag: ClassTag[A]): Class[A] =
    tag.runtimeClass.asInstanceOf[Class[A]]
}
