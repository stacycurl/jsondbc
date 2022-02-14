package jsondbc
package migration
package data

import jsondbc.SPI.Codec
import monocle.Traversal


case class Optic[Json](value: String)(traversal: Traversal[Json, Json]) {
  def apply(json: Json, operation: Operation[Json]): Json = traversal.modify(operation.apply)(json)
}

object Optic {
  implicit def opticCodec[Json: SPI]: Codec[Optic[Json], Json] =
    Codec[String, Json].xmap[Optic[Json]](Optic.create)(_.value)

  def create[Json: SPI](value: String): Optic[Json] =
    Optic(value)(JsonPath.traversal(Traversal.id[Json], value))

  def optics[Json: SPI](value: String): List[Optic[Json]] = JsonPath.relativeAncestors(value).map {
    case (token, traversal) => Optic(token)(traversal)
  }

  def empty[Json]: Optic[Json] = Optic("$")(Traversal.id[Json])
}