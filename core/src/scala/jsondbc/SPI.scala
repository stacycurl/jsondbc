package jsondbc

import monocle.{Prism, Traversal}
import monocle.function.{Each, FilterIndex}


trait SPI[Json] {
  type JsonObject

  def jNull: Json
  def jBoolean(value: Boolean): Json
  def jDouble(value: Double): Option[Json]
  def jLong(value: Long): Json
  def jString(value: String): Json
  def jField(json: Json, name: String): Option[Json]

  val ordering: Ordering[Json]

  def jObjectPrism:       Prism[Json, JsonObject]
  def jArrayPrism:        Prism[Json, List[Json]]
  def objectValuesOrArrayElements: Traversal[Json, Json]

  def jObjectEach:        Each[JsonObject, Json]
  def jObjectFilterIndex: FilterIndex[JsonObject, String, Json]
}

object SPI {
  def apply[JSON](implicit spi: SPI[JSON]): Aux[JSON, spi.JsonObject] = spi

  type Aux[Json, JsonObject0] = SPI[Json] { type JsonObject = JsonObject0 }
}