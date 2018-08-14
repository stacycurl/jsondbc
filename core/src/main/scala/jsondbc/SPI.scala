package jsondbc

import monocle.{Iso, Prism, Traversal}


trait SPI[J] {
  type JsonObject
  type JsonNumber

  // Helpers
  final def filterKeys(j: J, p: String => Boolean): J = mapMap(j, _.filter { case (key, _) => p(key) })
  final def filterKeysNot(j: J, p: String => Boolean): J = mapMap(j, _.filterNot { case (key, _) => p(key) })
  final def filterValues(j: J, p: J => Boolean): J = mapMap(j, _.filter { case (_, v) => p(v) })
  final def filterValuesNot(j: J, p: J => Boolean): J = mapMap(j, _.filterNot { case (_, v) => p(v) })

  final def renameFields(j: J, fromTos: (String, String)*): J = mapMap(j, map => {
    fromTos.foldLeft(map) {
      case (acc, (from, to)) => acc.get(from).fold(acc)(value => (acc - from) + ((to, value)))
    }
  })

  final def addIfMissing(j: J, assocs: (String, J)*): J = mapMap(j, map => {
    assocs.foldLeft(map) {
      case (acc, kv@(k, _)) => acc.get(k).fold(acc + kv)(_ ⇒ acc)
    }
  })

  private def mapMap(j: J, f: Map[String, J] => Map[String, J]): J =
    (jObject composeIso jObjectMap).modify(f).apply(j)

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
  def jObjectMap:  Iso[JsonObject, Map[String, J]]

  def jDescendants:  Traversal[J, J]
  def jObjectValues: Traversal[JsonObject, J]

  def filterObject(p: String => Boolean): Traversal[JsonObject, J]
}

object SPI {
  def apply[J](implicit spi: SPI[J]): Aux[J, spi.JsonObject] = spi

  type Aux[J, JsonObject0] = SPI[J] { type JsonObject = JsonObject0 }
}