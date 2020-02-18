package jsondbc.syntax

import jsondbc.{Descendant, SPI}
import monocle.Traversal


object generic {
  implicit class AnyFrills[A](val self: A) extends AnyVal {
    def descendant[J](implicit C: SPI.Codec[A, J], S: SPI[J]): Descendant[A, J, J] = {
      val traversal: Traversal[A, J] = S.traversal(C)

      Descendant(self, List(traversal), () => List("" -> traversal))
    }

    def descendant[J](paths: String*)(implicit C: SPI.Codec[A, J], S: SPI[J]): Descendant[A, J, J] = {
      val traversal: Traversal[A, J] = S.traversal(C)

      Descendant(self,
              paths.map(    jsondbc.JsonPath.traversal[A, J](traversal, _))(collection.breakOut),
        () => paths.flatMap(jsondbc.JsonPath.ancestors[A, J](traversal, _))(collection.breakOut)
      )
    }

    def filterNulls                     (implicit spi: SPI[A]): A = filterRecursive(_ != spi.jNull(()))
    def filterRecursive(p: A => Boolean)(implicit spi: SPI[A]): A = spi.filterRecursive(self, p)

    def filterKeys(p: String => Boolean)   (implicit spi: SPI[A]): A = spi.filterKeys(self, p)
    def filterKeysNot(p: String => Boolean)(implicit spi: SPI[A]): A = spi.filterKeysNot(self, p)
    def filterValues(p: A => Boolean)      (implicit spi: SPI[A]): A = spi.filterValues(self, p)
    def filterValuesNot(p: A => Boolean)   (implicit spi: SPI[A]): A = spi.filterValuesNot(self, p)

    def removeFields(names: String*)(implicit spi: SPI[A]): A = spi.removeFields(self, names: _*)
    def renameFields(fromTos: (String, String)*)(implicit spi: SPI[A]): A = spi.renameFields(self, fromTos: _*)

    def addIfMissing(assocs: (String, A)*)(implicit spi: SPI[A]): A = spi.addIfMissing(self, assocs: _*)

    def mapValuesWithKey(f: String => A => A)(implicit spi: SPI[A]): A = spi.mapValuesWithKey(self, f)

//    def delete(path: String): Json = {
//      path.split("/").toList.reverse match {
//        case head :: Nil ⇒ descendant("").obj.delete(head)
//        case head :: tail ⇒ descendant(tail.reverse.mkString("/")).obj.delete(head)
//        case _ ⇒ Json.jNull
//      }
//    }
  }
}
