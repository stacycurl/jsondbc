package jsondbc

import jsondbc.SPI.Aux
import jsondbc.syntax.Descendant
import monocle.function.{Each, FilterIndex}
import monocle.{Iso, PIso, Prism, Traversal}

trait SPI[Json] {
  type JsonObject

  def jNull: Json
  def jBoolean(value: Boolean): Json
  def jDouble(value: Double): Option[Json]
  def jLong(value: Long): Json
  def jString(value: String): Json
  def jField(json: Json, name: String): Option[Json]

  val ordering: Ordering[Json]

  def jObjectPrism:       Prism[Json, JsonObject]
  def jArrayPrism:        Prism[Json, List[Json]]
  def objectValuesOrArrayElements: Traversal[Json, Json]

  def jObjectEach:        Each[JsonObject, Json]
  def jObjectFilterIndex: FilterIndex[JsonObject, String, Json]
}

object SPI extends ArgonautSPI {
  def apply[JSON](implicit spi: SPI[JSON]): Aux[JSON, spi.JsonObject] = spi

  type Aux[Json, JsonObject0] = SPI[Json] { type JsonObject = JsonObject0 }
}

trait ArgonautSPI {
  import argonaut.{Json, JsonObject}
  import argonaut.{JsonMonocle, JsonObjectMonocle}

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
}

trait CirceSPI {
  import io.circe.{Json, JsonObject}
  import io.circe.optics.{JsonOptics, JsonObjectOptics}

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