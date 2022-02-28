import jsondbc.optics.JPrism
import org.reflections.Reflections

import java.lang.reflect.Modifier
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
      new Reflections(prefix).getSubTypesOf(self).asScala.toList.filterNot(_.isAbstract)

    def isAbstract: Boolean =
      Modifier.isAbstract(self.getModifiers)
  }

  implicit class PrismSyntax[S, A](private val self: JPrism[S, A]) extends AnyVal {
//    def toList: Prism[List[From], List[To]] =
//      Prism[List[From], List[To]](la ⇒ Some(la.flatMap(self.getOption)))(_.map(self.reverseGet))

//    def toMap[K]: Prism[Map[K, S], Map[K, A]] = Prism[Map[K, S], Map[K, A]](mapKA ⇒ {
//      Some(for {
//        (k, v) <- mapKA
//        to    <- self.getOption(v)
//      } yield k -> to)
//    })((mapKB: Map[K, A]) ⇒ {
//      mapKB.map {
//        case (k, v) => k -> self.reverseGet(v)
//      }
//    })
  }

  def className[A: ClassTag]: String =
    klassOf[A].getName

  def klassOf[A](implicit tag: ClassTag[A]): Class[A] =
    tag.runtimeClass.asInstanceOf[Class[A]]
}
