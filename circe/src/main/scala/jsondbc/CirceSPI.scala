package jsondbc

import io.circe.optics.{JsonObjectOptics, JsonOptics}
import io.circe.{Json, JsonNumber, JsonObject}
import monocle.{Iso, Prism, Traversal}
import jsondbc.SPI.Aux


// Investigate if later versions of circe can be used with earlier versions of circe optics (that use scalaz based monocle)
object CirceSPI extends CirceSPI
trait CirceSPI {
  implicit val circeSPI: Aux[Json, JsonObject, JsonNumber] = new SPI[Json] {
    type JsonObject = io.circe.JsonObject
    type JsonNumber = io.circe.JsonNumber

    def jField(json: Json, name: String): Option[Json] = for {
      obj <- json.asObject
      field <- obj(name)
    } yield field

    val ordering: Ordering[Json] = {
      Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[Json](json ⇒ {
        (json.asBoolean, json.asNumber.flatMap(_.toInt), json.asNumber.map(_.toDouble), json.asString)
      })
    }

    val jNull:       Prism[Json, Unit]       = JsonOptics.jsonNull
    val jObject:     Prism[Json, JsonObject] = JsonOptics.jsonObject
    val jArray:      Prism[Json, List[Json]] = JsonOptics.jsonArray composeIso Iso[Vector[Json], List[Json]](_.toList)(_.toVector)
    val jBoolean:    Prism[Json, Boolean]    = JsonOptics.jsonBoolean
    val jNumber:     Prism[Json, JsonNumber] = JsonOptics.jsonNumber
    val jDouble:     Prism[Json, Double]     = JsonOptics.jsonDouble
    val jString:     Prism[Json, String]     = JsonOptics.jsonString
    val jBigDecimal: Prism[Json, BigDecimal] = JsonOptics.jsonBigDecimal
    val jBigInt:     Prism[Json, BigInt]     = JsonOptics.jsonBigInt
    val jLong:       Prism[Json, Long]       = JsonOptics.jsonLong
    val jInt:        Prism[Json, Int]        = JsonOptics.jsonInt
    val jShort:      Prism[Json, Short]      = JsonOptics.jsonShort
    val jByte:       Prism[Json, Byte]       = JsonOptics.jsonByte

    val jObjectMap:  Iso[JsonObject, Map[String, Json]] =
      Iso[JsonObject, Map[String, Json]](_.toMap)(JsonObject.fromMap)

    val jDescendants:  Traversal[Json, Json]       = JsonOptics.jsonDescendants
    val jObjectValues: Traversal[JsonObject, Json] = JsonObjectOptics.jsonObjectEach.each

    def filterObject(p: String => Boolean): Traversal[JsonObject, Json] = JsonObjectOptics.jsonObjectFilterIndex.filterIndex(p)
  }
}
