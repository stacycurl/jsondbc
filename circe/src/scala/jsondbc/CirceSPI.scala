package jsondbc

import io.circe.optics.{JsonObjectOptics, JsonOptics}
import io.circe.{Json, JsonObject}
import monocle.function.{Each, FilterIndex}
import monocle.{Iso, Prism, Traversal}

import jsondbc.SPI.Aux


object CirceSPI extends CirceSPI
trait CirceSPI {
  implicit val circeSPI: Aux[Json, JsonObject] = new SPI[Json] {
    type JsonObject = io.circe.JsonObject

    val jNull: Json = Json.Null
    def jBoolean(value: Boolean): Json = Json.fromBoolean(value)
    def jDouble(value: Double): Option[Json] = Json.fromDouble(value)
    def jLong(value: Long): Json = Json.fromLong(value)
    def jString(value: String): Json = Json.fromString(value)

    def jField(json: Json, name: String): Option[Json] = for {
      obj <- json.asObject
      field <- obj(name)
    } yield field

    val ordering: Ordering[Json] = {
      Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[Json](json ⇒ {
        (json.asBoolean, json.asNumber.flatMap(_.toInt), json.asNumber.map(_.toDouble), json.asString)
      })
    }

    val jObjectPrism:       Prism[Json, JsonObject]               = JsonOptics.jsonObject
    val jArrayPrism:        Prism[Json, List[Json]]               = JsonOptics.jsonArray composeIso Iso[Vector[Json], List[Json]](_.toList)(_.toVector)
    val objectValuesOrArrayElements: Traversal[Json, Json]        = JsonOptics.jsonDescendants

    val jObjectEach:        Each[JsonObject, Json]                = JsonObjectOptics.jsonObjectEach
    val jObjectFilterIndex: FilterIndex[JsonObject, String, Json] = JsonObjectOptics.jsonObjectFilterIndex
  }
}
