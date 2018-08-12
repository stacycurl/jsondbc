package jsondbc

import argonaut.{CodecJson, Json, JsonMonocle, JsonNumber, JsonObject, JsonObjectMonocle}
import jsondbc.syntax.Descendant
import monocle.function.{Each, FilterIndex}
import monocle.{Iso, Prism, Traversal}
import jsondbc.SPI.Aux

object ArgonautSPI extends ArgonautSPI
trait ArgonautSPI {
  implicit val argonautSPI: Aux[Json, JsonObject] = new SPI[Json] {
    type JsonObject = argonaut.JsonObject

    val jNull: Json = Json.jNull
    def jBoolean(value: Boolean): Json = Json.jBool(value)
    def jDouble(value: Double): Option[Json] = Json.jNumber(value)
    def jLong(value: Long): Json = Json.jNumber(value)
    def jString(value: String): Json = Json.jString(value)
    def jField(json: Json, name: String): Option[Json] = json.field(name)

    val ordering: Ordering[Json] = {
      Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[Json](json ⇒ {
        (json.bool, json.number.flatMap(_.toInt), json.number.flatMap(_.toDouble), json.string)
      })
    }

    val jObjectPrism:       Prism[Json, JsonObject]               = JsonMonocle.jObjectPrism
    val jArrayPrism:        Prism[Json, List[Json]]               = JsonMonocle.jArrayPrism
    val objectValuesOrArrayElements: Traversal[Json, Json]        = Descendant.objectValuesOrArrayElements

    val jObjectEach:        Each[JsonObject, Json]                = JsonObjectMonocle.jObjectEach
    val jObjectFilterIndex: FilterIndex[JsonObject, String, Json] = JsonObjectMonocle.jObjectFilterIndex
  }

  implicit val cpfJsonToBoolean:    CanPrismFrom[Json, Boolean,    Boolean]    = CanPrismFrom(JsonMonocle.jBoolPrism)
  implicit val cpfJsonToJsonNumber: CanPrismFrom[Json, JsonNumber, JsonNumber] = CanPrismFrom(JsonMonocle.jNumberPrism)
  implicit val cpfJsonToString:     CanPrismFrom[Json, String,     String]     = CanPrismFrom(JsonMonocle.jStringPrism)
  implicit val cpfJsonToJsonArray:  CanPrismFrom[Json, List[Json], List[Json]] = CanPrismFrom(JsonMonocle.jArrayPrism)
  implicit val cpfJsonToJsonObject: CanPrismFrom[Json, JsonObject, JsonObject] = CanPrismFrom(JsonMonocle.jObjectPrism)
  implicit val cpfJsonToBigDecimal: CanPrismFrom[Json, BigDecimal, BigDecimal] = CanPrismFrom(JsonMonocle.jBigDecimalPrism)
//  implicit val cpfJsonToDouble:     jsondbc.CanPrismFrom[Json, Double,     Double]     = CanPrismFrom(jDoublePrism)
//  implicit val cpfJsonToFloat:      jsondbc.CanPrismFrom[Json, Float,      Float]      = CanPrismFrom(jFloatPrism)
  implicit val cpfJsonToBigInt:     CanPrismFrom[Json, BigInt,     BigInt]     = CanPrismFrom(JsonMonocle.jBigIntPrism)
  implicit val cpfJsonToLong:       CanPrismFrom[Json, Long,       Long]       = CanPrismFrom(JsonMonocle.jLongPrism)
  implicit val cpfJsonToInt:        CanPrismFrom[Json, Int,        Int]        = CanPrismFrom(JsonMonocle.jIntPrism)
  implicit val cpfJsonToShort:      CanPrismFrom[Json, Short,      Short]      = CanPrismFrom(JsonMonocle.jShortPrism)
  implicit val cpfJsonToByte:       CanPrismFrom[Json, Byte,       Byte]       = CanPrismFrom(JsonMonocle.jBytePrism)

  implicit def cpfJsonToCodec[A: CodecJson]: CanPrismFrom[Json, A, A] = {
    val A = CodecJson.derived[A]

    CanPrismFrom(Prism[Json, A](json => A.decodeJson(json).toOption)(A.encode))
  }

  implicit def cpfJsonObjectToTypedMap[V](implicit cpf: CanPrismFrom[Json, V, V])
    : CanPrismFrom[JsonObject, V, Map[String, V]] = CanPrismFrom(jsonObjectMapIso.composePrism(cpf.toMap[String].prism))

  private val jsonObjectMapIso: Iso[JsonObject, Map[String, Json]] =
    Iso[JsonObject, Map[String, Json]](_.toMap)(map ⇒ JsonObject.fromTraversableOnce(map))
}
