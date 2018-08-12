package jsondbc

import monocle.{Prism, Traversal}
import monocle.function.{Each, FilterIndex}


trait SPI[Json] {
  type JsonObject
  type JsonNumber

  def jField(json: Json, name: String): Option[Json]

  val ordering: Ordering[Json]

  def jNull:       Prism[Json, Unit]
  def jObject:     Prism[Json, JsonObject]
  def jArray:      Prism[Json, List[Json]]
  def jBoolean:    Prism[Json, Boolean]
  def jNumber:     Prism[Json, JsonNumber]
  def jDouble:     Prism[Json, Double]
  def jString:     Prism[Json, String]
  def jBigDecimal: Prism[Json, BigDecimal]
  def jBigInt:     Prism[Json, BigInt]
  def jLong:       Prism[Json, Long]
  def jInt:        Prism[Json, Int]
  def jShort:      Prism[Json, Short]
  def jByte:       Prism[Json, Byte]

  def jDescendants:       Traversal[Json, Json]
  def jObjectEach:        Each[JsonObject, Json]
  def jObjectFilterIndex: FilterIndex[JsonObject, String, Json]
}

object SPI {
  def apply[JSON](implicit spi: SPI[JSON]): Aux[JSON, spi.JsonObject] = spi

  type Aux[Json, JsonObject0] = SPI[Json] { type JsonObject = JsonObject0 }
}