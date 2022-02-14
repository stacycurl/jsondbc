package jsondbc

import argonaut._
import jsondbc.syntax.argonaut._

class ArgonautJsonSpec extends AbstractJsonSpec[Json] with ArgonautJsonUtil

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

  implicit val addressCodecJson: CodecJson[Address] =
    CodecJson.derived[List[String]].xmap[Address](Address(_))(_.lines)

  implicit val bananamanCodecJson: CodecJson[Bananaman] = CodecJson.casecodec9(Bananaman.apply, Bananaman.unapply)(
    "name", "lying", "age", "preferences", "address", "width", "knownUnknowns", "potatoes", "awkward"
  )
}
