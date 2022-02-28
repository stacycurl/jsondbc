package jsondbc
package migration
package data

import jsondbc.SPI.Codec
import jsondbc.optics.JTraversal


case class Optic[Json](value: String)(traversal: JTraversal[Json, Json]) {
  def apply(json: Json, operation: Operation[Json]): Json = traversal.modify(operation.apply)(json)
}

object Optic {
  implicit def opticCodec[Json: SPI]: Codec[Optic[Json], Json] =
    Codec[String, Json].xmap[Optic[Json]](Optic.create)(_.value)

  def create[Json: SPI](value: String): Optic[Json] =
    Optic(value)(JsonPath.traversal(SPI[Json].idTraversal[Json], value))

  def optics[Json: SPI](value: String): List[Optic[Json]] = JsonPath.relativeAncestors(value).map {
    case (token, traversal) => Optic(token)(traversal)
  }

  def empty[Json: SPI]: Optic[Json] =
    Optic("$")(SPI[Json].idTraversal[Json])
}