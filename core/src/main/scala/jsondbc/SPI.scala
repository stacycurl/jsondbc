package jsondbc

import monocle.{Iso, Optional, Prism, Traversal}


trait SPI[J] {
  type JsonObject
  type JsonNumber

  // Helpers
  final def filterKeys(j: J, p: String => Boolean): J = mapMap(j, _.filter { case (key, _) => p(key) })
  final def filterKeysNot(j: J, p: String => Boolean): J = mapMap(j, _.filterNot { case (key, _) => p(key) })
  final def filterValues(j: J, p: J => Boolean): J = mapMap(j, _.filter { case (_, v) => p(v) })
  final def filterValuesNot(j: J, p: J => Boolean): J = mapMap(j, _.filterNot { case (_, v) => p(v) })

  final def filterRecursive(j: J, p: J => Boolean): J = if (p(j)) {
    val mapMapped = mapMap(j, _.collect {
      case (k, v) if p(v) => k -> filterRecursive(v, p)
    })

    mapList(mapMapped, _.collect {
      case v if p(v) => filterRecursive(v, p)
    })
  } else {
    jNull.apply(())
  }

  final def removeFields(j: J, names: String*): J = mapMap(j, _.filter {
    case (field, _) => !names.contains(field)
  })

  final def retainFields(j: J, names: String*): J = mapMap(j, _.filter {
    case (field, _) => names.contains(field)
  })

  final def renameFields(j: J, fromTos: (String, String)*): J = renameFields(j, fromTos.toMap)

  final def renameFields(j: J, fromTos: Map[String, String]): J = mapMap(j, map => map.map {
    case (field, value) => fromTos.getOrElse(field, field) -> value
  })

  final def addIfMissing(j: J, assocs: (String, J)*): J = addIfMissing(j, assocs.toMap)

  final def addIfMissing(j: J, assocs: Map[String, J]): J = mapMap(j, map => assocs ++ map)

  final def reverse(j: J): J = Function.chain(Seq(
    jString.modify(_.reverse)(_),
    jArray.modify(_.reverse)(_)
  )).apply(j)

  final def mapValuesWithKey(j: J, f: String => J => J): J =
    mapMap(j, _.map { case (k, v) => (k, f(k)(v)) })

  final def jObject(entries: (String, J)*): J =
    jObject(entries.toMap)

  final def jObject(entries: Map[String, J]): J =
    jObjectEntries.apply(entries)

  final def jArray(entries: J*): J =
    jArray.apply(entries.toList)

  def jField(json: J, name: String): Option[J]

  def ordering: Ordering[J]

  def js: Prism[J, String] = jString

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

  def jObjectEntries: Prism[J, Map[String, J]] =
    jObject composeIso jObjectMap

  def jStrings: Prism[J, List[String]] =
    jArray composeIso Iso[List[J], List[String]](_.flatMap(jString.getOption))(_.map(jString.apply))

  def filterObject(p: String => Boolean): Traversal[JsonObject, J]

  final def traversal[A](codec: SPI.Codec[A, J]): Traversal[A, J] =
    Traversal.id[A] composeOptional optional(codec)

  final def optional[A](codec: SPI.Codec[A, J]): Optional[A, J] =
    Optional[A, J](a => Some(codec.encode(a)))(j => oldA => codec.decode(j).getOrElse(oldA))

  private def mapList(j: J, f: List[J] => List[J]): J =
    jArray.modify(f).apply(j)

  private def mapMap(j: J, f: Map[String, J] => Map[String, J]): J =
    (jObject composeIso jObjectMap).modify(f).apply(j)
}

object SPI {
  def apply[J](implicit spi: SPI[J]): Aux[J, spi.JsonObject, spi.JsonNumber] = spi

  type Aux[J, JsonObject0, JsonNumber0] = SPI[J] {
    type JsonObject = JsonObject0
    type JsonNumber = JsonNumber0
  }

  trait Codec[A, J] {
    def encode(a: A): J
    def decode(j: J): Either[String, A]

    final def xmap[B](f: A => B)(g: B => A): Codec[B, J] = Codec.XMappedCodec(this, f, g)
  }

  object Codec {
    def apply[A, J](implicit c: Codec[A, J]): Codec[A, J] = c

    implicit def identityCodec[J]: Codec[J, J] = new IdentityCodec[J]

    private class IdentityCodec[J] extends Codec[J, J] {
      def encode(j: J): J = j
      def decode(j: J) = Right(j)
    }

    private case class XMappedCodec[A, B, J](from: Codec[A, J], f: A => B, g: B => A) extends Codec[B, J] {
      def encode(b: B): J = from.encode(g(b))
      def decode(j: J): Either[String, B] = from.decode(j).right.map(f)
    }
  }
}
