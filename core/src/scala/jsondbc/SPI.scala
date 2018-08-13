package jsondbc

import monocle.{Prism, Traversal}
import monocle.function.{Each, FilterIndex}


trait SPI[J] {
  type JsonObject
  type JsonNumber

  def jField(json: J, name: String): Option[J]

  val ordering: Ordering[J]

  def jNull:       Prism[J, Unit]
  def jObject:     Prism[J, JsonObject]
  def jArray:      Prism[J, List[J]]
  def jBoolean:    Prism[J, Boolean]
  def jNumber:     Prism[J, JsonNumber]
  def jDouble:     Prism[J, Double]
  def jString:     Prism[J, String]
  def jBigDecimal: Prism[J, BigDecimal]
  def jBigInt:     Prism[J, BigInt]
  def jLong:       Prism[J, Long]
  def jInt:        Prism[J, Int]
  def jShort:      Prism[J, Short]
  def jByte:       Prism[J, Byte]

  def jDescendants:       Traversal[J, J]
  def jObjectEach:        Each[JsonObject, J]
  def jObjectFilterIndex: FilterIndex[JsonObject, String, J]
}

object SPI {
  def apply[J](implicit spi: SPI[J]): Aux[J, spi.JsonObject] = spi

  type Aux[J, JsonObject0] = SPI[J] { type JsonObject = JsonObject0 }
}