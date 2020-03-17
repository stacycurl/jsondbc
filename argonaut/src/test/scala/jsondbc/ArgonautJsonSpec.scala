package jsondbc

import argonaut._
import jsondbc.syntax.argonaut._
import jsondbc.syntax.generic._
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.argonaut.matchers._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps

class ArgonautJsonSpec extends AbstractJsonSpec[Json] with ArgonautJsonUtil {

  "migrations codec" in {
    val flat = Migration(
      "$.foods.fruit"  -> Migration.UpperCase,
      "$.foods.staple" -> Migration.Trim,
      "$.foods.staple" -> Migration.Reverse,
      "$.foods"        -> Migration.Rename("staple" -> "carbs")
    )

    flat shouldRoundTripTo Json.array(
      obj("$.foods.fruit"  := "upper-case"),
      obj("$.foods.staple" := "trim"),
      obj("$.foods.staple" := "reverse"),
      obj("$.foods"        := obj("rename" := obj("staple" := "carbs")))
    )

    val nested = Migration(
      "$" -> Migration(
        "$.foods" -> Migration(
          "$.fruit"  -> Migration.UpperCase,
          "$.staple" -> Migration(
            "$" -> Migration.Trim,
            "$" -> Migration.Reverse
          ),
          "$" -> Migration.Rename("staple" -> "carbs")
        )
      )
    )

    val n = Migration.nested(
      "$.foods.fruit"  -> Migration.UpperCase,
      "$.foods.staple" -> Migration.Trim,
      "$.foods.staple" -> Migration.Reverse,
      "$.foods"        -> Migration.Rename("staple" -> "carbs")
    )

    n <=> nested

    nested shouldRoundTripTo Json.array(
      obj("$" := List(
        obj("$.foods" := List(
          obj("$.fruit"  := "upper-case"),
          obj("$.staple" := List(
            obj("$" := "trim"),
            obj("$" := "reverse")
          )),
          obj("$" := obj("rename" := obj("staple" := "carbs")))
        ))
      ))
    )

    val foods = obj(
      "foods" := obj(
        "fruit"  := "banana",
        "staple" := "potato "
      )
    )

    val migratedFoods = obj(
      "foods" := obj(
        "fruit" := "BANANA",
        "carbs" := "otatop"
      )
    )

    flat.apply(foods)   <=> migratedFoods
    nested.apply(foods) <=> migratedFoods
  }

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
}

trait ArgonautJsonUtil extends JsonUtil[Json] {
  protected def pretty(json: Json): String = json.spaces2

  def parse(jsonText: String): Json = Parse.parseOption(jsonText).getOrElse(sys.error("not json"))

  def obj(socks: Json.JsonAssoc*): Json = Json.jObjectFields(socks: _*)

  def reverse[A](decodeResult: DecodeResult[List[A]]): DecodeResult[List[A]] = decodeResult.map(_.reverse)

  protected def append(to: Json, assoc: (String, Json)): Json = assoc ->: to

  override protected def delta(actual: Json, expected: Json): Json =
    sjc.delta.argonaut.json.actualExpected.flat.delta(actual, expected)

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

  implicit val addressCodec: CodecJson[Address] =
    CodecJson.derived[List[String]].xmap[Address](Address)(_.lines)

  implicit val bananamanCodec: CodecJson[Bananaman] = CodecJson.casecodec9(Bananaman.apply, Bananaman.unapply)(
    "name", "lying", "age", "preferences", "address", "width", "knownUnknowns", "potatoes", "awkward"
  )
}
