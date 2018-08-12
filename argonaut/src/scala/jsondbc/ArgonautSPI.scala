package jsondbc

import argonaut.{Json, JsonMonocle, JsonObject, JsonObjectMonocle}
import jsondbc.syntax.Descendant
import monocle.function.{Each, FilterIndex}
import monocle.{Prism, Traversal}

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
}
