package jsondbc

import io.circe.optics.{JsonObjectOptics, JsonOptics}
import io.circe.{Json, JsonNumber, JsonObject}
import jsondbc.SPI.Aux
import jsondbc.optics.monocle.cats.{CatsMonocleOptics, IsoAdapter, PrismAdapter, TraversalAdapter}
import monocle.function.{Each, FilterIndex}
import monocle.{Iso, Traversal}


// Investigate if later versions of circe can be used with earlier versions of circe optics (that use scalaz based monocle)
object CirceSPI extends CirceSPI

trait CirceSPI {
  implicit val circeSPI: Aux[Json, JsonObject, JsonNumber] = new SPI[Json] with CatsMonocleOptics {
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

    val jNull:       PrismAdapter[Json, Unit]       = PrismAdapter(JsonOptics.jsonNull)
    val jObject:     PrismAdapter[Json, JsonObject] = PrismAdapter(JsonOptics.jsonObject)
    val jArray:      PrismAdapter[Json, List[Json]] = PrismAdapter(JsonOptics.jsonArray composeIso Iso[Vector[Json], List[Json]](_.toList)(_.toVector))
    val jBoolean:    PrismAdapter[Json, Boolean]    = PrismAdapter(JsonOptics.jsonBoolean)
    val jNumber:     PrismAdapter[Json, JsonNumber] = PrismAdapter(JsonOptics.jsonNumber)
    val jDouble:     PrismAdapter[Json, Double]     = PrismAdapter(JsonOptics.jsonDouble)
    val jString:     PrismAdapter[Json, String]     = PrismAdapter(JsonOptics.jsonString)
    val jBigDecimal: PrismAdapter[Json, BigDecimal] = PrismAdapter(JsonOptics.jsonBigDecimal)
    val jBigInt:     PrismAdapter[Json, BigInt]     = PrismAdapter(JsonOptics.jsonBigInt)
    val jLong:       PrismAdapter[Json, Long]       = PrismAdapter(JsonOptics.jsonLong)
    val jInt:        PrismAdapter[Json, Int]        = PrismAdapter(JsonOptics.jsonInt)
    val jShort:      PrismAdapter[Json, Short]      = PrismAdapter(JsonOptics.jsonShort)
    val jByte:       PrismAdapter[Json, Byte]       = PrismAdapter(JsonOptics.jsonByte)

    val jObjectMap:  IsoAdapter[JsonObject, Map[String, Json]] =
      iso[JsonObject, Map[String, Json]](_.toMap)(JsonObject.fromMap)

    val jDescendants:  TraversalAdapter[Json, Json]       = TraversalAdapter(JsonOptics.jsonDescendants)
    val jObjectValues: TraversalAdapter[JsonObject, Json] = TraversalAdapter(JsonObjectOptics.jsonObjectEach.each)

    def filterObject(p: String => Boolean): TraversalAdapter[JsonObject, Json] =
      TraversalAdapter(JsonObjectOptics.jsonObjectFilterIndex.filterIndex(p))

    def idTraversal[A]: TraversalAdapter[A, A] =
      TraversalAdapter(Traversal.id[A])

    def listTraversal[A]: TraversalAdapter[List[A], A] =
      TraversalAdapter(Each.each(Each.listEach))

    def filterIndexTraversal[A](p: Int ⇒ Boolean): TraversalAdapter[List[A], A] =
      TraversalAdapter(FilterIndex.filterIndex(p)(FilterIndex.listFilterIndex))
  }
}
