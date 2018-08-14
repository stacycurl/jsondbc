package jsondbc.syntax

import jsondbc.{Descendant, SPI}
import monocle.Traversal


object generic {
  implicit class AnyFrills[A](val self: A) extends AnyVal {
    def descendant(implicit spi: SPI[A]): Descendant[A, A, A] =
      Descendant(self, List(Traversal.id[A]), () => List("" -> Traversal.id[A]))

    def descendant(paths: String*)(implicit spi: SPI[A]): Descendant[A, A, A] = Descendant(self,
      paths.map(jsondbc.JsonPath.traversal[A])(collection.breakOut),
      () => paths.flatMap(jsondbc.JsonPath.ancestors[A])(collection.breakOut)
    )

    def filterNulls                     (implicit spi: SPI[A]): A = filterRecursive(_ != spi.jNull(()))
    def filterRecursive(p: A => Boolean)(implicit spi: SPI[A]): A = spi.filterRecursive(self, p)

    def filterKeys(p: String => Boolean)   (implicit spi: SPI[A]): A = spi.filterKeys(self, p)
    def filterKeysNot(p: String => Boolean)(implicit spi: SPI[A]): A = spi.filterKeysNot(self, p)
    def filterValues(p: A => Boolean)      (implicit spi: SPI[A]): A = spi.filterValues(self, p)
    def filterValuesNot(p: A => Boolean)   (implicit spi: SPI[A]): A = spi.filterValuesNot(self, p)

    def renameFields(fromTos: (String, String)*)(implicit spi: SPI[A]): A = spi.renameFields(self, fromTos: _*)

    def addIfMissing(assocs: (String, A)*)(implicit spi: SPI[A]): A= spi.addIfMissing(self, assocs: _*)
  }
}
