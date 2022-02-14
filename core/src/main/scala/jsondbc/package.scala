import monocle.Prism
import org.reflections.Reflections

import scala.collection.JavaConverters.asScalaSetConverter
import scala.collection.immutable.List
import scala.reflect.ClassTag
import scala.util.matching.Regex

package object jsondbc {
  implicit class RegexMatcher(private val self: StringContext) extends AnyVal {
    def r: Regex = self.parts.mkString("(.+)")
      .replaceAllLiterally("?", "\\?")
      .r
  }

  implicit class ClassSyntax[A](private val self: Class[A]) extends AnyVal {
    def implementationsIn(prefix: String): List[Class[_ <: A]] =
      new Reflections(prefix).getSubTypesOf(self).asScala.toList
  }

  implicit class PrismSyntax[From, To](private val self: Prism[From, To]) extends AnyVal {
    def toList: Prism[List[From], List[To]] =
      Prism[List[From], List[To]](la ⇒ Some(la.flatMap(self.getOption)))(_.map(self.reverseGet))

    def toMap[K]: Prism[Map[K, From], Map[K, To]] = Prism[Map[K, From], Map[K, To]](mapKA ⇒ {
      Some(for {
        (k, v) <- mapKA
        to    <- self.getOption(v)
      } yield k -> to)
    })((mapKB: Map[K, To]) ⇒ {
      mapKB.map {
        case (k, v) => k -> self.reverseGet(v)
      }
    })
  }

  def className[A: ClassTag]: String =
    klassOf[A].getName

  def klassOf[A](implicit tag: ClassTag[A]): Class[A] =
    tag.runtimeClass.asInstanceOf[Class[A]]
}
