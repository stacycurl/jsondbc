package jsondbc
package util

import scala.reflect.ClassTag


trait ClassResolver {
  def resolve[A: ClassTag](clazz: Class[_]): A
}

object ClassResolver {
  implicit val defaultResolver: ClassResolver = ObjectResolver

  object ObjectResolver extends ClassResolver {
    def resolve[A: ClassTag](clazz: Class[_]): A = {
      import scala.reflect.runtime.universe

      val traitClass: Class[_] = implicitly[ClassTag[A]].runtimeClass

      require(
        requirement = traitClass.isAssignableFrom(clazz),
        message = s"${clazz.getName} does not implement ${className[A]}"
      )

      val runtimeMirror = universe.runtimeMirror(clazz.getClassLoader)

      try {
        runtimeMirror.reflectModule(runtimeMirror.staticModule(clazz.getName)).instance.asInstanceOf[A]
      } catch {
        case e: ClassNotFoundException => throw new IllegalArgumentException(s"${clazz.getName} is not an object", e)
      }
    }
  }
}