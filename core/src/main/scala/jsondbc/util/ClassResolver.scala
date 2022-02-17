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

      if (!traitClass.isAssignableFrom(clazz))
        throw new IllegalArgumentException(s"${clazz.getName} does not implement ${className[A]}")

      if (clazz.isAbstract)
        throw new IllegalArgumentException(s"${clazz.getName} is abstract")

      val runtimeMirror = universe.runtimeMirror(clazz.getClassLoader)

      try {
        runtimeMirror.reflectModule(runtimeMirror.staticModule(clazz.getName)).instance.asInstanceOf[A]
      } catch {
        case e: ClassNotFoundException => throw new IllegalArgumentException(s"${clazz.getName} is not an object", e)
      }
    }
  }
}