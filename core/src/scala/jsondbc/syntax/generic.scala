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
  }
}
