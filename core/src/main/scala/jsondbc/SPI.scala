package jsondbc

import jsondbc.syntax._
import jsondbc.util.Extractor
import monocle.{Iso, Optional, Prism, Traversal}


// TODO: Remove dependency on monocle in core, it's probably limiting the versions of non-scalaz based json libs.
trait SPI[J] {
  type JsonObject
  type JsonNumber

  def ordering: Ordering[J]

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

  def jField(json: J, name: String): Option[J]
  def filterObject(p: String => Boolean): Traversal[JsonObject, J]


  // Helpers
  def filterKeys(j: J, p: String => Boolean): J = mapMap(j, _.filter { case (key, _) => p(key) })
  def filterKeysNot(j: J, p: String => Boolean): J = mapMap(j, _.filterNot { case (key, _) => p(key) })
  def filterValues(j: J, p: J => Boolean): J = mapMap(j, _.filter { case (_, v) => p(v) })
  def filterValuesNot(j: J, p: J => Boolean): J = mapMap(j, _.filterNot { case (_, v) => p(v) })

  def filterRecursive(j: J, p: J => Boolean): J = if (!p(j)) jNull.apply(()) else {
    val mapMapped = mapMap(j, _.collect {
      case (k, v) if p(v) => k -> filterRecursive(v, p)
    })

    mapList(mapMapped, _.collect {
      case v if p(v) => filterRecursive(v, p)
    })
  }

  def removeFields(j: J, names: String*): J = mapMap(j, _.filter {
    case (field, _) => !names.contains(field)
  })

  def retainFields(j: J, names: String*): J = mapMap(j, _.filter {
    case (field, _) => names.contains(field)
  })

  def renameFields(j: J, fromTos: (String, String)*): J = renameFields(j, fromTos.toMap)

  def renameFields(j: J, fromTos: Map[String, String]): J = mapMap(j, map => map.map {
    case (field, value) => fromTos.getOrElse(field, field) -> value
  })

  def addIfMissing(j: J, assocs: (String, J)*): J = addIfMissing(j, assocs.toMap)

  def addIfMissing(j: J, assocs: Map[String, J]): J = mapMap(j, map => assocs ++ map)

  def upsert(j: J, assocs: (String, J)*): J = upsert(j, assocs.toMap)

  def upsert(j: J, assocs: Map[String, J]): J = mapMap(j, map => map ++ assocs)

  def reverse(j: J): J = Function.chain(Seq(
    jString.modify(_.reverse)(_),
    jArray.modify(_.reverse)(_)
  )).apply(j)

  def mapValuesWithKey(j: J, f: String => J => J): J =
    mapMap(j, _.map { case (k, v) => (k, f(k)(v)) })

  def obj(entries: (String, J)*): J =
    obj(entries.toMap)

  def obj(entries: Map[String, J]): J =
    jObjectEntries.apply(entries)

  def arr(entries: J*): J =
    jArray.apply(entries.toList)

  def js: Prism[J, String] =
    jString

  def jObjectEntries: Prism[J, Map[String, J]] =
    jObject composeIso jObjectMap

  def jEntries[E](entryPrism: Prism[J, E]): Prism[J, Map[String, E]] =
    jObjectEntries composePrism entryPrism.toMap[String]

  def jStrings: Prism[J, List[String]] =
    jArrayEntries[String](jString)

  def jArrayEntries[E](elementPrism: Prism[J, E]): Prism[J, List[E]] =
    jArray composePrism elementPrism.toList

  def traversal[A](codec: SPI.Codec[A, J]): Traversal[A, J] =
    Traversal.id[A] composeOptional optional(codec)

  def optional[A](codec: SPI.Codec[A, J]): Optional[A, J] =
    Optional[A, J](a => Some(codec.encode(a)))(j => oldA => codec.decode(j).getOrElse(oldA))

  def reversePrism[A](codec: SPI.Codec[A, J]): Prism[J, A] =
    Prism[J, A](j ⇒ codec.decode(j).toOption)(codec.encode(_))

  private def mapList(j: J, f: List[J] => List[J]): J =
    jArray.modify(f).apply(j)

  private def mapMap(j: J, f: Map[String, J] => Map[String, J]): J =
    jObjectEntries.modify(f).apply(j)
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
    def apply[A, JJ](implicit c: Codec[A, JJ]): Codec[A, JJ] = c

    def apply[CC, A, JJ](apply: A => CC, CC: Extractor[CC, A])(
      an: String
    )(implicit spi: SPI[JJ], ac: Codec[A, JJ]): Codec[CC, JJ] = new Codec[CC, JJ] {
      def encode(cc: CC): JJ = cc match {
        case CC(a) ⇒ spi.obj(
          an := a
        )
      }

      def decode(j: JJ): Either[String, CC] = for {
        entries ← spi.jObjectEntries.unapply(j).toRight("Expected an object")
        a ← entries.decode[A](an)
      } yield apply(a)
    }

    def apply[CC, A, B, JJ](apply: (A,B) => CC, CC: Extractor[CC, (A,B)])(
      an: String, bn: String
    )(implicit spi: SPI[JJ], ac: Codec[A, JJ], bc: Codec[B, JJ]): Codec[CC, JJ] = new Codec[CC, JJ] {
      def encode(cc: CC): JJ = cc match {
        case CC(a, b) ⇒ spi.obj(
          an := a,
          bn := b
        )
      }

      def decode(j: JJ): Either[String, CC] = for {
        entries ← spi.jObjectEntries.unapply(j).toRight("Expected an object")
        a ← entries.decode[A](an)
        b ← entries.decode[B](bn)
      } yield apply(a, b)
    }

    def apply[CC, A, B, C, D, E, F, G, H, I, JJ](apply: (A,B,C,D,E,F,G,H,I) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G,H,I)])(
      an: String, bn: String, cn: String, dn: String, en: String, fn: String, gn: String, hn: String, in: String
    )(implicit spi: SPI[JJ],
      ac: Codec[A, JJ],
      bc: Codec[B, JJ],
      ccodec: Codec[C, JJ],
      dc: Codec[D, JJ],
      ec: Codec[E, JJ],
      fc: Codec[F, JJ],
      gc: Codec[G, JJ],
      hc: Codec[H, JJ],
      ic: Codec[I, JJ]
    ): Codec[CC, JJ] = new Codec[CC, JJ] {
      def encode(cc: CC): JJ = cc match {
        case CC(a, b, c, d, e, f, g, h, i) ⇒ spi.obj(
          an := a,
          bn := b,
          cn := c,
          dn := d,
          en := e,
          fn := f,
          gn := g,
          hn := h,
          in := i
        )
      }

      def decode(j: JJ): Either[String, CC] = for {
        entries ← spi.jObjectEntries.unapply(j).toRight("Expected an object")
        a ← entries.decode[A](an)
        b ← entries.decode[B](bn)
        c ← entries.decode[C](cn)
        d ← entries.decode[D](dn)
        e ← entries.decode[E](en)
        f ← entries.decode[F](fn)
        g ← entries.decode[G](gn)
        h ← entries.decode[H](hn)
        i ← entries.decode[I](in)
      } yield apply(a, b, c, d, e, f, g, h, i)
    }

    private implicit class MapCodecSyntax[J](private val self: Map[String, J]) extends AnyVal {
      def decode[A](an: String)(implicit codec: Codec[A, J]): Either[String, A] = for {
        aj ← self.get(an).toRight(s"Expected object to contain $an")
        a  ← codec.decode(aj)
      } yield a
    }

    implicit def identityCodec[J: SPI]: Codec[J, J] = new IdentityCodec[J]

    implicit def listCodec[A, J](implicit spi: SPI[J], elementCodec: Codec[A, J]): Codec[List[A], J] =
      PrismCodec[List[A], J]("Expected a list", spi.jArray composePrism spi.reversePrism[A](elementCodec).toList)

    implicit def mapCodec[V, J](implicit spi: SPI[J], k: Codec[V, J]): Codec[Map[String, V], J] =
      PrismCodec[Map[String, V], J]("Expected a map", spi.jEntries[V](spi.reversePrism(k)))

    implicit def stringCodec[J](implicit spi: SPI[J]): Codec[String, J] =
      PrismCodec("Expected a string", spi.jString)

    implicit def booleanCodec[J](implicit spi: SPI[J]): Codec[Boolean, J] =
      PrismCodec("Expected a boolean", spi.jBoolean)

    implicit def intCodec[J](implicit spi: SPI[J]): Codec[Int, J] =
      PrismCodec("Expected an int", spi.jInt)

    implicit def doubleCodec[J](implicit spi: SPI[J]): Codec[Double, J] =
      PrismCodec("Expected a double", spi.jDouble)

    private case class PrismCodec[A, J](error: String, prism: Prism[J, A]) extends Codec[A, J] {
      def encode(a: A): J = prism(a)

      def decode(j: J): Either[String, A] = prism.unapply(j).toRight(error)
    }

    private class IdentityCodec[J] extends Codec[J, J] {
      def encode(j: J): J = j
      def decode(j: J): Right[String, J] = Right(j)
    }

    private case class XMappedCodec[A, B, J](from: Codec[A, J], f: A => B, g: B => A) extends Codec[B, J] {
      def encode(b: B): J = from.encode(g(b))
      def decode(j: J): Either[String, B] = from.decode(j).right.map(f)
    }
  }
}

//    object JString {
//      def unapply[J](value: J)(implicit spi: SPI[J]): Option[String] =
//        spi.jString.getOption(value)
//    }
//
