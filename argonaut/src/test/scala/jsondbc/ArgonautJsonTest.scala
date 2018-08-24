package jsondbc

//import argonaut.Json._
import argonaut.StringWrap.StringToStringWrap
import argonaut._
import jsondbc.SPI.Aux
import jsondbc.syntax.argonaut._
import org.scalatest.FreeSpecLike
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.argonaut.matchers._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps

import jsondbc.syntax.generic._

class ArgonautJsonTest extends AbstractJsonTest[Json] with ArgonautJsonUtil {
  import spi._

  "descendant_case_class" - {
    val bananaMan = Bananaman(
      "Eric", lying = true, 3, Map("bananas" -> true),
      Address(List("29 Acacia Road", "Nuttytown")), 33.5, Map(), Nil, Map("1" -> "one")
    )

    "complex" in {
      val actual = bananaMan
        .descendant("$.preferences.*").bool.set(false)
        .descendant("$.address").array.string.modify("Flat B" :: _)
        .descendant("$.address[*]").string.modify(_.toUpperCase)
        .descendant("$.potatoes.*.variety").string.modify(_ ⇒ "Avalanche")
        .descendant("$.knownUnknowns.*").int.modify(_ ⇒ 42)
        .descendant("$.awkward.*").string.modify(_.toUpperCase)

      actual <=> Bananaman(
        "Eric", lying = true, 3, Map("bananas" -> false),
        Address(List("FLAT B", "29 ACACIA ROAD", "NUTTYTOWN")), 33.5, Map(), Nil, Map("1" -> "ONE")
      )
    }

    "dynamic" in {
      val actual = bananaMan
        .descendant.preferences.each.bool.set(false)
        .descendant.address.array.string.modify("Flat B" :: _)
        .descendant.address.each.string.modify(_.toUpperCase)
        .descendant.potatoes.each.variety.string.modify(_ ⇒ "Avalanche")
        .descendant.knownUnknowns.each.int.modify(_ ⇒ 42)
        .descendant.awkward.each.string.modify(_.toUpperCase)

      actual <=> Bananaman(
        "Eric", lying = true, 3, Map("bananas" -> false),
        Address(List("FLAT B", "29 ACACIA ROAD", "NUTTYTOWN")), 33.5, Map(), Nil, Map("1" -> "ONE")
      )
    }
  }

  "as" in {
    jobj.descendant("$.address").as[Address].getAll            <=> List(Address(List("29 Acacia Road", "Nuttytown")))
    jobj.descendant("$.address").as[Address].modify(_.reverse) <=> jobj.descendant("$.address").array.modify(_.reverse)
  }

  "delete" in {//    println(parse(
//      """{
//        |   "a": {
//        |     "nested": {
//        |       "thing": "bye bye"
//        |     }
//        |   },
//        |   "remainder": "still here"
//        |}
//      """.stripMargin).delete("a/nested/thing").spaces2)
//
//    println(parse("""{"candy": "lollipop", "noncandy": null,"other": "things"}""")
//      .descendant("candy").string.set("big turks").filterNulls
//      .delete("other").spaces2)
//

//    store.jsonPath("$.store.book[*].author").getAll.foreach(j ⇒ println(j.spaces2))



    val conditions = parse("""{ "conditions":
          			[
          				{ "id": "i1", "condition": true },
          				{ "id": "i2", "condition": false }
          			]
          		}""")

    Json.jArray(conditions.descendant("$.conditions[?(@['condition'] == true)].id").getAll)  <=> parse("""["i1"]""")
    Json.jArray(conditions.descendant("$.conditions[?(@['condition'] == false)].id").getAll) <=> parse("""["i2"]""")

    conditions.descendant("$.conditions[?(@['condition'] == true)]").modify(_.addIfMissing("matched" := true)) <=> parse("""{
      "conditions": [
        { "id": "i1", "condition": true, "matched": true },
        { "id": "i2", "condition": false }
      ]
    }""")



    val objConditions = parse("""{ "conditions":
        {
          "first": { "id": "i1", "condition": true },
          "second": { "id": "i2", "condition": false }
        }
      }""")

    Json.jArray(objConditions.descendant("$.conditions[?(@['condition'] == true)].id").getAll)  <=> parse("""["i1"]""")
    Json.jArray(objConditions.descendant("$.conditions[?(@['condition'] == false)].id").getAll) <=> parse("""["i2"]""")

    objConditions.descendant("$.conditions[?(@['condition'] == true)]").modify(_.addIfMissing("matched" := true)) <=> parse("""{
      "conditions": {
        "first": { "id": "i1", "condition": true, "matched": true },
        "second": { "id": "i2", "condition": false }
      }
    }""")
  }
}

trait ArgonautJsonUtil extends JsonUtil[Json] {
  def print(j: Json): Unit = println(j.spaces2)

  def parse(jsonText: String): Json = Parse.parseOption(jsonText).getOrElse(sys.error("not json"))

  def obj(socks: Json.JsonAssoc*): Json = Json.jObjectFields(socks: _*)

  def reverse[A](decodeResult: DecodeResult[List[A]]): DecodeResult[List[A]] = decodeResult.map(_.reverse)

  protected def append(to: Json, assoc: (String, Json)): Json = assoc ->: to

  protected def assertJsonEquals(actual: Json, expected: Json): Unit =
    anyDeltaMatcherOps(actual) <=> expected

  val codec: CodecJson[List[String]]           = CodecJson.derived[List[String]]
  val mapCodec: CodecJson[Map[String, String]] = CodecJson.derived[Map[String, String]]
  val stringCodec: CodecJson[String]           = CodecJson.derived[String]
  val (encoder, decoder)       = (codec.Encoder, codec.Decoder)
  val (mapEncoder, mapDecoder) = (mapCodec.Encoder, mapCodec.Decoder)
  trait Base
  object Base { val encoder = EncodeJson[Base]({ case d: Derived ⇒ Derived.codec.encode(d) }) }
  case class Derived(i: Int) extends Base
  object Derived { implicit val codec: CodecJson[Derived] = CodecJson.casecodec1(Derived.apply, Derived.unapply)("i") }
  val derived = Derived(123)
  val derivedEncoded = Derived.codec.encode(derived)

  case class Bananaman(
    name: String, lying: Boolean, age: Int, preferences: Map[String, Boolean], address: Address,
    width: Double, knownUnknowns: Map[String, String], potatoes: List[String], awkward: Map[String, String]
  )

  object Bananaman {
    implicit val bananamanCodec: CodecJson[Bananaman] = CodecJson.casecodec9(Bananaman.apply, Bananaman.unapply)(
      "name", "lying", "age", "preferences", "address", "width", "knownUnknowns", "potatoes", "awkward"
    )
  }

  case class Address(lines: List[String]) {
    def reverse: Address = copy(lines.reverse)
  }

  object Address {
    implicit val addressCodec: CodecJson[Address] =
      CodecJson.derived[List[String]].xmap[Address](Address(_))(_.lines)
  }
}
