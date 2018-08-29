package jsondbc

import argonaut._
import jsondbc.syntax.argonaut._
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.argonaut.matchers._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps

import jsondbc.syntax.generic._

class ArgonautJsonSpec extends AbstractJsonSpec[Json] with ArgonautJsonUtil {
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

  implicit val addressCodec: CodecJson[Address] =
    CodecJson.derived[List[String]].xmap[Address](Address(_))(_.lines)

  implicit val bananamanCodec: CodecJson[Bananaman] = CodecJson.casecodec9(Bananaman.apply, Bananaman.unapply)(
    "name", "lying", "age", "preferences", "address", "width", "knownUnknowns", "potatoes", "awkward"
  )
}
