package jsondbc

import argonaut.Json._
import argonaut._
import argonaut.StringWrap.StringToStringWrap
import org.scalatest.{FreeSpec, FreeSpecLike}
import sjc.delta.argonaut.json.actualExpected.flat._
import sjc.delta.argonaut.matchers._
import sjc.delta.matchers.syntax.anyDeltaMatcherOps
import jsondbc.syntax.argonaut._

class JsonTest extends FreeSpec with JsonUtil {
  "booleanFilter" in {
    val json = parse(
      """{
        | "people": [
        |  {"person": {"name": "Arnie", "age": 100}, "address": "California"},
        |  {"person": {"name": "Raymond", "age": 21}, "address": "Brisvegas"},
        |  {"person": {"name": "Raymond", "age": 35}, "address": "London"}
        | ]
        |}
      """.stripMargin)

    val x: List[Json] =
      json.descendant("$.people[?(@.person.name == 'Raymond' && @.person.age == 21)].address").getAll

    json.descendant("$.people[?(@.person.name == 'Raymond' && @.person.age == 21)].address").getAll <=> List(jString("Brisvegas"))
    json.descendant("$.people[?(@.person.name == 'Arnie' || @.person.age == 21)].address").getAll <=> List(jString("California"), jString("Brisvegas"))

    json.descendant("$.people[?(@.person.name == 'Raymond' && @.person.age != 21)].address").getAll <=> List(jString("London"))
  }

  "foo" in {
    Json("people" := List(
      Json("name" := "Bob", "width" := 200, "height" := 100),
      Json("name" := "Jim", "width" := 100, "height" := 100),
      Json("name" := "Sue", "width" := 100, "height" := 200)
    )).descendant("$.people[?(@.width > @.height)]").modify(_.addIfMissing("description" := "pancake"))
      .descendant("$.people[?(@.width == @.height)]").modify(_.addIfMissing("description" := "round"))
      .descendant("$.people[?(@.width < @.height)]").modify(_.addIfMissing("description" := "normal")) <=> Json("people" := List(
      Json("name" := "Bob", "width" := 200, "height" := 100, "description" := "pancake"),
      Json("name" := "Jim", "width" := 100, "height" := 100, "description" := "round"),
      Json("name" := "Sue", "width" := 100, "height" := 200, "description" := "normal")
    ))
  }

  "filterNulls" in {
    test(_.filterNulls,
      """null"""                        → """null""",
      """{ "a": null, "b": 3 }"""       → """{ "b": 3 }""",
      """[ "a", null, "b" ]"""          → """[ "a", "b" ]""",
      """{ "o": [ "a", null, "b" ] }""" → """{ "o": [ "a", "b" ] }""",
      """[ { "a": null, "b": 3 } ]"""   → """[ { "b": 3 } ]"""
    )
  }

  "filterR" in {
    test(_.filterR(_ != Json.jEmptyObject),
      """{}"""                        → """null""",
      """{ "a": {}, "b": 3 }"""       → """{ "b": 3 }""",
      """[ "a", {}, "b" ]"""          → """[ "a", "b" ]""",
      """{ "o": [ "a", {}, "b" ] }""" → """{ "o": [ "a", "b" ] }""",
      """[ { "a": {}, "b": 3 } ]"""   → """[ { "b": 3 } ]"""
    )
  }

  "descendant_values" in {
    jobj.descendant("age").getAll <=> List(age)
    jobj.descendant("age").modify(_ ⇒ redacted) <=> ("age" → redacted) ->: jobj

    jobj.descendant("{name, age}").getAll <=> List(name, age)
    jobj.descendant("{name, age}").modify(_ ⇒ redacted) <=> ("name" → redacted) ->: ("age" → redacted) ->: jobj

    jobj.descendant("age").int.getAll <=> List(3)
    jobj.descendant("age").int.modify(_ * 2) <=> ("age" → jNumber(6)) ->: jobj
  }

  "descendant_multiple" in {
    jobj.descendant("name", "age").getAll <=> List(name, age)
  }

  "descendant_elements" in {
    jArray(fields).descendant("[0, 2]").getAll <=> List(lying, address)

    jArray(fields).descendant("[0, 2]").modify(_ ⇒ redacted) <=> jArrayElements(
      redacted, name, redacted, age, width, preferences, potatoes, knownUnknowns, awkward
    )
  }

  "descendant_all" in {
    jobj.descendant("*").getAll <=> List(name, age, lying, address, preferences, width, potatoes, knownUnknowns, awkward)

    jobj.descendant("*").modify(_ ⇒ jString("redacted")) <=> jObjectFields(
      "name" → redacted, "age" → redacted, "lying" → redacted, "address" → redacted, "preferences" → redacted,
      "width" → redacted, "potatoes" → redacted, "knownUnknowns" → redacted, "awkward" → redacted
    )
  }

  "descendant_ancestors" in {
    jobj.descendant("$.preferences.bananas").string.ancestors <=> jObjectFields(
      "$"                     -> Json.jArray(jobj.descendant("$").getAll),
      "$.preferences"         -> Json.jArray(jobj.descendant("$.preferences").getAll),
      "$.preferences.bananas" -> Json.jArray(jobj.descendant("$.preferences.bananas").getAll)
    )

    jobj.descendant("preferences/bananas").string.ancestors <=> jObjectFields(
      ""                    -> Json.jArray(jobj.descendant("").getAll),
      "preferences"         -> Json.jArray(jobj.descendant("preferences").getAll),
      "preferences/bananas" -> Json.jArray(jobj.descendant("preferences/bananas").getAll)
    )
  }

  "descendant_firstEmptyAt" in {
    jobj.descendant("$.preferences.bananas").firstEmptyAt <=> None
    jobj.descendant("$.preferences.apples") .firstEmptyAt <=> Some("$.preferences.apples")
    jobj.descendant("$.pref.apples")        .firstEmptyAt <=> Some("$.pref")
  }

  "as" in {
    jobj.descendant("$.address").as[Address].getAll            <=> List(Address(List("29 Acacia Road", "Nuttytown")))
    jobj.descendant("$.address").as[Address].modify(_.reverse) <=> jobj.descendant("$.address").array.modify(_.reverse)
  }

  "descendant_complex" in {
    jobj.descendant("preferences/*").bool.set(false)
        .descendant("address").array.string.modify("Flat B" :: _)
        .descendant("address/*").string.modify(_.toUpperCase)
        .descendant("potatoes/*/variety").string.modify(_ ⇒ "Avalanche")
        .descendant("knownUnknowns/*").int.modify(_ ⇒ 42)
        .descendant("awkward/*").string.modify(_.toUpperCase) <=> parse("""
          |{
          |  "name" : "Eric",
          |  "lying" : true,
          |  "age" : 3,
          |  "preferences" : {
          |    "bananas" : false
          |  },
          |  "address" : [
          |    "FLAT B",
          |    "29 ACACIA ROAD",
          |    "NUTTYTOWN"
          |  ],
          |  "width" : 33.5,
          |  "knownUnknowns" : {},
          |  "potatoes" : [],
          |  "awkward" : { "1": "ONE" }
          |}""".stripMargin
        )
  }

  "descendant_dynamic_complex" in {
    jobj.descendant.preferences.each.bool.set(false)
        .descendant.address.array.string.modify("Flat B" :: _)
        .descendant.address.each.string.modify(_.toUpperCase)
        .descendant.potatoes.each.variety.string.modify(_ ⇒ "Avalanche")
        .descendant.knownUnknowns.each.int.modify(_ ⇒ 42)
        .descendant.awkward.each.string.modify(_.toUpperCase) <=> parse("""
          |{
          |  "name" : "Eric",
          |  "lying" : true,
          |  "age" : 3,
          |  "preferences" : {
          |    "bananas" : false
          |  },
          |  "address" : [
          |    "FLAT B",
          |    "29 ACACIA ROAD",
          |    "NUTTYTOWN"
          |  ],
          |  "width" : 33.5,
          |  "knownUnknowns" : {},
          |  "potatoes" : [],
          |  "awkward" : { "1": "ONE" }
          |}""".stripMargin
        )
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

  private def print(values: List[Json]) = values.foreach(j ⇒ println(j.spaces2))


  private val store = parse(
    """
      |{
      |    "store": {
      |        "book": [
      |            {
      |                "category": "reference",
      |                "author": "Nigel Rees",
      |                "title": "Sayings of the Century",
      |                "price": 8.95
      |            },
      |            {
      |                "category": "fiction",
      |                "author": "Evelyn Waugh",
      |                "title": "Sword of Honour",
      |                "price": 12.99
      |            },
      |            {
      |                "category": "fiction",
      |                "author": "Herman Melville",
      |                "title": "Moby Dick",
      |                "isbn": "0-553-21311-3",
      |                "price": 8.99
      |            },
      |            {
      |                "category": "fiction",
      |                "author": "J. R. R. Tolkien",
      |                "title": "The Lord of the Rings",
      |                "isbn": "0-395-19395-8",
      |                "price": 22.99
      |            }
      |        ],
      |        "bicycle": {
      |            "color": "red",
      |            "price": 19.95
      |        }
      |    },
      |    "expensive": 10
      |}
    """.stripMargin)

  "workWithBooleanFilters" in {    val json = parse("""{ "conditions": [true, false, true] }""")

    json.descendant("$.conditions[?(@ == true)]").getAll  <=> List(jTrue, jTrue)
    json.descendant("$.conditions[?(@ == false)]").getAll <=> List(jFalse)
    json.descendant("$.conditions[?(false == @)]").getAll <=> List(jFalse)
  }

  "`work with test set 3`" in {    val json = parse("""{ "points": [
          				             { "id":"i1", "x": 4, "y":-5 },
          				             { "id":"i2", "x":-2, "y": 2, "z":1 },
          				             { "id":"i3", "x": 8, "y": 3 },
          				             { "id":"i4", "x":-6, "y":-1 },
          				             { "id":"i5", "x": 0, "y": 2, "z":1 },
          				             { "id":"i6", "x": 1, "y": 4 }
          				           ]
          				         }""")

    json.descendant("$.points[1]").getAll <=> List(parse("""{ "id":"i2", "x":-2, "y": 2, "z":1 }"""))
    json.descendant("$.points[4].x").getAll <=> List(jNumber(0))
    json.descendant("$.points[?(@['id']=='i4')].x").getAll <=> List(jNumber(-6))
    json.descendant("$.points[*].x").getAll <=> List(4, -2, 8, -6, 0, 1).map(jNumber)
    // Non supported syntax "$['points'][?(@['x']*@['x']+@['y']*@['y'] > 50)].id"
    json.descendant("$['points'][?(@['y'] >= 3)].id").getAll <=> List(jString("i3"), jString("i6"))
    json.descendant("$.points[?(@['z'])].id").getAll <=> List(jString("i2"), jString("i5"))
    json.descendant("$.points[?(@.z)].id").getAll <=> List(jString("i2"), jString("i5"))
    // Non supported syntax "$.points[(count(@)-1)].id"
  }

  "Multi-fields accessors should be interpreted correctly" in {
    val json = parse("""{"menu":{"year":2013,"file":"open","options":[{"bold":true},{"font":"helvetica"},{"size":3}]}}""")
    json.descendant("$.menu['file','year']").getAll <=> List(jNumber(2013), jString("open"))
    json.descendant("$.menu.options['foo','bar']").getAll <=> Nil
    json.descendant("$.menu.options[*]['bold','size']").getAll <=> List(jTrue, jNumber(3))
//    json.descendant("$..options['foo','bar']").getAll <=> Nil
//    json.descendant("$..options[*]['bold','size']").getAll <=> List(jTrue, jNumber(3))
  }

  "workWithDeepPredicates" in {
    val json = parse(
      """{
        | "people": [
        |  {"person": {"name": "Arnie", "age": 100}, "address": "California"},
        |  {"person": {"name": "Raymond", "age": 21}, "address": "Brisvegas"}
        | ]
        |}
      """.stripMargin)

    json.descendant("$.people[?(@.person.name == 'Arnie')].address").getAll <=> List(jString("California"))
  }

  "descendant_renameFields" in {
    parse("""{ "thing": { "original": true } }""").descendant("thing").renameFields("original" -> "renamed") <=> parse("""{ "thing": { "renamed": true } }""")
  }

  "descendant_obj_renameFields" in {
    parse("""{ "thing": { "original": true } }""").descendant("thing").obj.renameFields("original" -> "renamed") <=> parse("""{ "thing": { "renamed": true } }""")
  }

  "renameFields" in {
    jObjectFields("original" → jTrue).renameFields("original" -> "renamed") <=> jObjectFields("renamed" → jTrue)
  }

  "descendant_renameManyFields" in {
    parse("""{ "thing": { "a": true, "b": false} }""").descendant("thing").renameFields("a" → "A", "b" → "B") <=> parse("""{ "thing": { "A": true, "B": false} }""")
  }

  "descendant_obj_renameManyFields" in {
    parse("""{ "thing": { "a": true, "b": false} }""").descendant("thing").obj.renameFields("a" → "A", "b" → "B") <=> parse("""{ "thing": { "A": true, "B": false} }""")
  }

  "renameManyFields" in {
    jObjectFields("a" → jTrue, "b" → jFalse).renameFields("a" → "A", "b" → "B") <=> jObjectFields("A" → jTrue, "B" → jFalse)
  }

  "descendant_addIfMissing" in {
    on(
      parse("""{ "thing": {} }"""),           parse("""{ "thing": {"a": true} }""")
    ).calling(_.descendant("thing").addIfMissing("a" := jFalse)).produces(
      parse("""{ "thing": {"a": false} }"""), parse("""{ "thing": {"a": true} }""")
    )
  }

  "descendant_obj_addIfMissing" in {
    on(
      parse("""{ "thing": {} }"""),           parse("""{ "thing": {"a": true} }""")
    ).calling(_.descendant("thing").obj.addIfMissing("a" := jFalse)).produces(
      parse("""{ "thing": {"a": false} }"""), parse("""{ "thing": {"a": true} }""")
    )
  }

  "addIfMissing" in {
    on(
      jEmptyObject,      obj("a" := existing)
    ).calling(_.addIfMissing("a" := jString(added))).produces(
      obj("a" := added), obj("a" := existing)
    )
  }

  "descendant_addIfMissing_many" in {
    on(
      thing(jEmptyObject),         thing(obj("a" := existing)),
      thing(obj("b" := existing)), thing(obj("a" := existing, "b" := existing))
    ).calling(_.descendant("thing").addIfMissing("a" := added, "b" := added)).produces(
      thing(obj("a" := added, "b" := added)),    thing(obj("a" := existing, "b" := added)),
      thing(obj("a" := added, "b" := existing)), thing(obj("a" := existing, "b" := existing))
    )
  }

  "descendant_obj_addIfMissing_many" in {
    on(
      thing(jEmptyObject),         thing(obj("a" := existing)),
      thing(obj("b" := existing)), thing(obj("a" := existing, "b" := existing))
    ).calling(_.descendant("thing").obj.addIfMissing("a" := added, "b" := added)).produces(
      thing(obj("a" := added, "b" := added)),    thing(obj("a" := existing, "b" := added)),
      thing(obj("a" := added, "b" := existing)), thing(obj("a" := existing, "b" := existing))
    )
  }

  "addIfMissing_many" in {
    on(
      jEmptyObject,         obj("a" := existing),
      obj("b" := existing), obj("a" := existing, "b" := existing)
    ).calling(_.addIfMissing("a" := added, "b" := added)).produces(
      obj("a" := added, "b" := added),    obj("a" := existing, "b" := added),
      obj("a" := added, "b" := existing), obj("a" := existing, "b" := existing)
    )
  }

  "removeFields" in {
    on(
      obj("a" := "value", "b" := 123, "c" := true), obj("a" := "value", "b" := 123)
    ).calling(_.removeFields("b", "c")).produces(
      obj("a" := "value"), obj("a" := "value")
    )
  }

  private def test(f: Json ⇒ Json, data: (String, String)*): Unit = data.foreach {
    case (input, expected) ⇒ f(parse(input)) <=> parse(expected)
  }

  private def thing(value: Json): Json = obj("thing" → value)

  private val added    = "added"
  private val existing = "existing"
}

class JsonObjectTest extends JsonUtil with FreeSpecLike {
  "renameFields" in {
    obj("original" := true)       .withObject(_.renameFields("original" -> "renamed")) <=> obj("renamed" := true)
    obj("a" := true, "b" := false).withObject(_.renameFields("a" → "A", "b" → "B"))    <=> obj("A" := true, "B" := false)
  }

  "addIfMissing" in {
    on(
      jEmptyObject,     obj("a" := existing)
    ).calling(_.withObject(_.addIfMissing("a" := added))).produces(
      obj("a" := added), obj("a" := existing)
    )
  }

  "addIfMissing_many" in {
    on(
      jEmptyObject,        obj("a" := existing),
      obj("b" := existing), obj("a" := existing, "b" := existing)
    ).calling(_.withObject(_.addIfMissing("a" := added, "b" := added))).produces(
      obj("a" := added, "b" := added),    obj("a" := existing, "b" := added),
      obj("a" := added, "b" := existing), obj("a" := existing, "b" := existing)
    )
  }

  private val added    = jString("added")
  private val existing = jString("existing")
}

trait JsonUtil extends FreeSpecLike {
  case class on[A](as: A*) {
    case class calling[B](fs: (A ⇒ B)*) {
      def produces(bs: B*): Unit = (for {f ← fs; a ← as} yield f(a)).toList === bs.toList
    }
  }

  def reverse(json: Json): Json = json.withArray(_.reverse)
  def reverse[A](decodeResult: DecodeResult[List[A]]): DecodeResult[List[A]] = decodeResult.map(_.reverse)

  val codec: CodecJson[List[String]]           = CodecJson.derived[List[String]]
  val mapCodec: CodecJson[Map[String, String]] = CodecJson.derived[Map[String, String]]
  val stringCodec: CodecJson[String]           = CodecJson.derived[String]
  val (encoder, decoder)       = (codec.Encoder, codec.Decoder)
  val (mapEncoder, mapDecoder) = (mapCodec.Encoder, mapCodec.Decoder)

  val list = List("food", "foo", "bard", "bar")
  val json = Json.jArray(list.map(Json.jString))
  def jsonMap(kvs: (String, String)*) = Json.jObjectAssocList(kvs.map { case (k, v) ⇒ (k, Json.jString(v)) }.toList)

  trait Base
  object Base { val encoder = EncodeJson[Base]({ case d: Derived ⇒ Derived.codec.encode(d) }) }

  case class Derived(i: Int) extends Base
  object Derived { implicit val codec: CodecJson[Derived] = CodecJson.casecodec1(Derived.apply, Derived.unapply)("i") }

  val derived = Derived(123)
  val derivedEncoded = Derived.codec.encode(derived)

  val acaciaRoad = List(jString("29 Acacia Road"), jString("Nuttytown"))
  val bananas = JsonObject.empty + ("bananas", jBool(true))
  val intObj = JsonObject.empty + ("1", jString("one"))

  val fields@List(lying, name, address, age, width, preferences, potatoes, knownUnknowns, awkward) = List(
    jBool(true), jString("Eric"), jArray(acaciaRoad), jNumber(3), jNumberOrNull(33.5), jObject(bananas),
    jArrayElements(), jObjectFields(), jObject(intObj)
  )

  val jobj: Json = jObjectFields(
    "name" → name, "age" → age, "lying" → lying, "address" → address, "preferences" → preferences, "width" → width,
    "potatoes" → potatoes, "knownUnknowns" → knownUnknowns, "awkward" → awkward
  )

  case class Address(lines: List[String]) {
    def reverse: Address = copy(lines.reverse)
  }

  object Address {
    implicit val addressCodec: CodecJson[Address] =
      CodecJson.derived[List[String]].xmap[Address](Address(_))(_.lines)
  }

  val redacted = jString("redacted")

  def parse(jsonText: String) = Parse.parseOption(jsonText).getOrElse(sys.error("not json"))

  def obj(socks: Json.JsonAssoc*): Json = Json.jObjectFields(socks: _*)

  def reverseEntry(key: String, value: String): (String, String) = (key.reverse, value.reverse)
}