package jsondbc
package util

import jsondbc.util.ClassResolver.ObjectResolver


class ClassResolverSpec extends JSpec {
  "ObjectResolver.resolve" - {
    "an object" in {
      assert(ObjectResolver.resolve[Trait](Object.getClass) === Object)
    }

    "an incompatible object" in {
      assertThrows[IllegalArgumentException](s"${className[IncompatibleObject.type]} does not implement ${className[Trait]}") {
        ObjectResolver.resolve[Trait](IncompatibleObject.getClass)
      }
    }

    "a class" in {
      assertThrows[IllegalArgumentException](s"${className[Class]} is not an object") {
        ObjectResolver.resolve[Trait](classOf[Class])
      }
    }

    "an abstract class" in {
      assertThrows[IllegalArgumentException](s"${className[AbstractClass]} is abstract") {
        ObjectResolver.resolve[Trait](classOf[AbstractClass])
      }
    }
  }
}


trait Trait
abstract class AbstractClass extends Trait
class Class extends Trait
object Object extends Trait
object IncompatibleObject