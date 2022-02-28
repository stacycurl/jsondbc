package jsondbc

import argonaut.{CodecJson, DecodeJson, EncodeJson, Json, JsonMonocle, JsonNumber, JsonObject, JsonObjectMonocle}
import jsondbc.SPI.Aux
import jsondbc.optics.monocle.scalaz.{IsoAdapter, PrismAdapter, ScalazMonocleOptics, TraversalAdapter}
import monocle.{Iso, Prism}

object ArgonautSPI extends ArgonautSPI
trait ArgonautSPI {
  implicit def argonautCodecs[A: EncodeJson: DecodeJson]: SPI.Codec[A, Json] = new SPI.Codec[A, Json] {
    def encode(a: A): Json = EncodeJson.of[A].encode(a)
    def decode(j: Json): Either[String, A] = DecodeJson.of[A].decodeJson(j).toEither.left.map(_._1)
  }

  implicit val argonautSPI: Aux[Json, JsonObject, JsonNumber] = new SPI[Json] with ScalazMonocleOptics {
    type JsonObject = argonaut.JsonObject
    type JsonNumber = argonaut.JsonNumber

    def jField(json: Json, name: String): Option[Json] = json.field(name)

    val ordering: Ordering[Json] = {
      Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[Json](json ⇒ {
        (json.bool, json.number.flatMap(_.toInt), json.number.flatMap(_.toDouble), json.string)
      })
    }

    val jNull:       PrismAdapter[Json, Unit]       = adapt(JsonMonocle.jNullPrism)
    val jObject:     PrismAdapter[Json, JsonObject] = adapt(JsonMonocle.jObjectPrism)
    val jArray:      PrismAdapter[Json, List[Json]] = adapt(JsonMonocle.jArrayPrism)
    val jBoolean:    PrismAdapter[Json, Boolean]    = adapt(JsonMonocle.jBoolPrism)
    val jNumber:     PrismAdapter[Json, JsonNumber] = adapt(JsonMonocle.jNumberPrism)
    val jString:     PrismAdapter[Json, String]     = adapt(JsonMonocle.jStringPrism)
    val jBigDecimal: PrismAdapter[Json, BigDecimal] = adapt(JsonMonocle.jBigDecimalPrism)
    val jBigInt:     PrismAdapter[Json, BigInt]     = adapt(JsonMonocle.jBigIntPrism)
    val jLong:       PrismAdapter[Json, Long]       = adapt(JsonMonocle.jLongPrism)
    val jInt:        PrismAdapter[Json, Int]        = adapt(JsonMonocle.jIntPrism)
    val jShort:      PrismAdapter[Json, Short]      = adapt(JsonMonocle.jShortPrism)
    val jByte:       PrismAdapter[Json, Byte]       = adapt(JsonMonocle.jBytePrism)

    val jDouble:     PrismAdapter[Json, Double] = adapt(Prism[Json, Double](_.fold(
      Some(Double.NaN), _ => None, _.toDouble, _ => None, _ => None, _ => None
    ))(Json.jNumberOrNull))

    val jObjectMap: IsoAdapter[JsonObject, Map[String, Json]] =
      adapt(Iso[JsonObject, Map[String, Json]](_.toMap)(JsonObject.fromIterable))

    val jDescendants:  TraversalAdapter[Json, Json]       = adapt(JsonMonocle.jDescendants)
    val jObjectValues: TraversalAdapter[JsonObject, Json] = adapt(JsonObjectMonocle.jObjectEach.each)

    def filterObject(p: String => Boolean): TraversalAdapter[JsonObject, Json] =
      adapt(JsonObjectMonocle.jObjectFilterIndex.filterIndex(p))
  }

  implicit val cpfJsonToJsonObject: CanPrismFrom[Json, JsonObject, JsonObject] = CanPrismFrom(argonautSPI.jObject)

  implicit def cpfJsonToCodec[A: CodecJson]: CanPrismFrom[Json, A, A] = {
    val A = CodecJson.derived[A]

    CanPrismFrom(PrismAdapter(Prism[Json, A](json => A.decodeJson(json).toOption)(A.encode)))
  }

  implicit def cpfJsonObjectToTypedMap[V](implicit cpf: CanPrismFrom[Json, V, V])
    : CanPrismFrom[JsonObject, V, Map[String, V]] = CanPrismFrom(jsonObjectMapIso.composePrism(cpf.toMap[String].prism))

  private val jsonObjectMapIso: jsondbc.optics.JIso[JsonObject, Map[String, Json]] =
    IsoAdapter(Iso[JsonObject, Map[String, Json]](_.toMap)(map ⇒ JsonObject.fromIterable(map)))
}
