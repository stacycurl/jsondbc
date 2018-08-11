package jsondbc

import argonaut.{JsonMonocle, JsonObjectMonocle}
import jsondbc.SPI.Aux
import jsondbc.syntax.Descendant
import monocle.function.{Each, FilterIndex}
import monocle.{Prism, Traversal}

trait SPI[Json] {
  type JsonObject

  def jNull: Json
  def jBoolean(value: Boolean): Json
  def jDouble(value: Double): Json
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

  implicit val argonautSPI: Aux[Json, JsonObject] = new SPI[Json] {
    type JsonObject = argonaut.JsonObject

    val jNull: Json = Json.jNull
    def jBoolean(value: Boolean): Json = Json.jBool(value)
    def jDouble(value: Double): Json = Json.jNumberOrNull(value)
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