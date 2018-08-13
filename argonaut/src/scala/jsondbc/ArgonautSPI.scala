package jsondbc

import argonaut.{CodecJson, Json, JsonMonocle, JsonObject, JsonObjectMonocle}
import jsondbc.SPI.Aux
import monocle.function.{Each, FilterIndex}
import monocle.{Iso, Lens, Prism, Traversal}

object ArgonautSPI extends ArgonautSPI
trait ArgonautSPI {
  implicit val argonautSPI: Aux[Json, JsonObject] = new SPI[Json] {
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

    val jDescendants:       Traversal[Json, Json]   = JsonMonocle.jDescendants
    val jObjectEach:        Each[JsonObject, Json]                = JsonObjectMonocle.jObjectEach
    val jObjectFilterIndex: FilterIndex[JsonObject, String, Json] = JsonObjectMonocle.jObjectFilterIndex
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
