package jsondbc

import argonaut.{CodecJson, Json, JsonMonocle, JsonNumber, JsonObject, JsonObjectMonocle}
import jsondbc.SPI.Aux
import monocle.{Iso, Prism, Traversal}

object ArgonautSPI extends ArgonautSPI
trait ArgonautSPI {
  implicit def argonautCodecs[A: CodecJson]: SPI.Codec[A, Json] = new SPI.Codec[A, Json] {
    def encode(a: A): Json = CodecJson.derived[A].encode(a)
    def decode(j: Json): Either[String, A] = CodecJson.derived[A].decodeJson(j).toEither.left.map(_._1)
  }

  implicit val argonautSPI: Aux[Json, JsonObject, JsonNumber] = new SPI[Json] {
    type JsonObject = argonaut.JsonObject
    type JsonNumber = argonaut.JsonNumber

    def jField(json: Json, name: String): Option[Json] = json.field(name)

    val ordering: Ordering[Json] = {
      Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[Json](json ⇒ {
        (json.bool, json.number.flatMap(_.toInt), json.number.flatMap(_.toDouble), json.string)
      })
    }

    val jNull:       Prism[Json, Unit]       = JsonMonocle.jNullPrism
    val jObject:     Prism[Json, JsonObject] = JsonMonocle.jObjectPrism
    val jArray:      Prism[Json, List[Json]] = JsonMonocle.jArrayPrism
    val jBoolean:    Prism[Json, Boolean]    = JsonMonocle.jBoolPrism
    val jNumber:     Prism[Json, JsonNumber] = JsonMonocle.jNumberPrism
    val jString:     Prism[Json, String]     = JsonMonocle.jStringPrism
    val jBigDecimal: Prism[Json, BigDecimal] = JsonMonocle.jBigDecimalPrism
    val jBigInt:     Prism[Json, BigInt]     = JsonMonocle.jBigIntPrism
    val jLong:       Prism[Json, Long]       = JsonMonocle.jLongPrism
    val jInt:        Prism[Json, Int]        = JsonMonocle.jIntPrism
    val jShort:      Prism[Json, Short]      = JsonMonocle.jShortPrism
    val jByte:       Prism[Json, Byte]       = JsonMonocle.jBytePrism

    val jDouble:     Prism[Json, Double] = Prism[Json, Double](_.fold(
      Some(Double.NaN), _ => None, _.toDouble, _ => None, _ => None, _ => None
    ))(Json.jNumberOrNull)

    val jObjectMap: Iso[JsonObject, Map[String, Json]] =
      Iso[JsonObject, Map[String, Json]](_.toMap)(JsonObject.fromTraversableOnce)

    val jDescendants:  Traversal[Json, Json]       = JsonMonocle.jDescendants
    val jObjectValues: Traversal[JsonObject, Json] = JsonObjectMonocle.jObjectEach.each

    def filterObject(p: String => Boolean): Traversal[JsonObject, Json] = JsonObjectMonocle.jObjectFilterIndex.filterIndex(p)
  }

  implicit val cpfJsonToJsonObject: CanPrismFrom[Json, JsonObject, JsonObject] = CanPrismFrom(argonautSPI.jObject)

  implicit def cpfJsonToCodec[A: CodecJson]: CanPrismFrom[Json, A, A] = {
    val A = CodecJson.derived[A]

    CanPrismFrom(Prism[Json, A](json => A.decodeJson(json).toOption)(A.encode))
  }

  implicit def cpfJsonObjectToTypedMap[V](implicit cpf: CanPrismFrom[Json, V, V])
    : CanPrismFrom[JsonObject, V, Map[String, V]] = CanPrismFrom(jsonObjectMapIso.composePrism(cpf.toMap[String].prism))

  private val jsonObjectMapIso: Iso[JsonObject, Map[String, Json]] =
    Iso[JsonObject, Map[String, Json]](_.toMap)(map ⇒ JsonObject.fromTraversableOnce(map))
}
