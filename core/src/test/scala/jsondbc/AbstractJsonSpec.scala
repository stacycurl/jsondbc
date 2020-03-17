package jsondbc

import jsondbc.syntax.generic._
import org.scalatest.{FreeSpecLike, Matchers}

abstract class AbstractJsonSpec[J: SPI] extends JsonUtil[J] with FreeSpecLike {
  import spi._

  val (j123, j456) = (jLong(123), jLong(456))
  val ab = obj("a" -> j123, "b" -> j456)

  "filterKeys" in {
    ab.filterKeys(_ == "a") <=> obj("a" -> j123)
  }

  "filterKeysNot" in {
    ab.filterKeysNot(_ == "a") <=> obj("b" -> j456)
  }

  "filterValues" in {
    ab.filterValues(_ == j123) <=> obj("a" -> j123)
  }

  "filterValuesNot" in {
    ab.filterValuesNot(_ == j123) <=> obj("b" -> j456)
  }


  "filterNulls" in {
    val jNull = spi.jNull(())

    List(
      jNull                                         -> jNull,
      obj("a" -> jNull, "b" -> j123)                -> obj("b" -> j123),
      jArray(List(jString("a"), jNull))             -> jArray(List(jString("a"))),
      obj("o" -> jArray(List(jString("a"), jNull))) -> obj("o" -> jArray(List(jString("a")))),
      jArray(List(obj("a" -> jNull, "b" -> j123)))  -> jArray(List(obj("b" -> j123)))
    ).foreach {
      case (input, expected) => input.filterNulls <=> expected
    }
  }

  "filterRecursive" in {
    val jEmpty = obj()

    List(
      jEmpty                                         -> jNull(()),
      obj("a" -> jEmpty, "b" -> j123)                -> obj("b" -> j123),
      jArray(List(jString("a"), jEmpty))             -> jArray(List(jString("a"))),
      obj("o" -> jArray(List(jString("a"), jEmpty))) -> obj("o" -> jArray(List(jString("a")))),
      jArray(List(obj("a" -> jEmpty, "b" -> j123)))  -> jArray(List(obj("b" -> j123)))
    ).foreach {
      case (input, expected) => input.filterRecursive(_ != jEmpty) <=> expected
    }
  }

  "renameFields" in {
    obj("original" → jTrue).renameFields("original" -> "renamed") <=> obj("renamed" → jTrue)
  }

  "removeFields" in {
    ab.removeFields("a") <=> obj("b" -> j456)
  }

  "retainFields" in {
    ab.retainFields("a") <=> obj("a" -> j123)
  }

  "addIfMissing" in {
    obj()                .addIfMissing("a" -> jAdded) <=> obj("a" -> jAdded)
    obj("a" -> jExisting).addIfMissing("a" -> jAdded) <=> obj("a" -> jExisting)
  }

  "descendant" - {
    val oab = obj("owner" -> ab)

    "values" in {
      jobj.descendant("$.age").getAll <=> List(age)
      jobj.descendant("$.age").modify(_ ⇒ redacted) <=> ("age" → redacted) ->: jobj

      jobj.descendant("$.name", "$.age").getAll <=> List(name, age)
      jobj.descendant("$.name", "$.age").modify(_ ⇒ redacted) <=> ("name" → redacted) ->: ("age" → redacted) ->: jobj

      jobj.descendant("$.age").int.getAll <=> List(3)
      jobj.descendant("$.age").int.modify(_ * 2) <=> ("age" → jInt(6)) ->: jobj
    }

    "elements" in {
      jArray(fields).descendant("$[0, 2]").getAll <=> List(lying, address)

      jArray(fields).descendant("$[0, 2]").modify(_ ⇒ redacted) <=> jArray(List(
        redacted, name, redacted, age, width, preferences, potatoes, knownUnknowns, awkward
      ))
    }

    "all" in {
      jobj.descendant("$.*").getAll.sorted <=> List(name, age, lying, address, preferences, width, potatoes, knownUnknowns, awkward).sorted

      jobj.descendant("$.*").modify(_ ⇒ jString("redacted")) <=> obj(
        "name" → redacted, "age" → redacted, "lying" → redacted, "address" → redacted, "preferences" → redacted,
        "width" → redacted, "potatoes" → redacted, "knownUnknowns" → redacted, "awkward" → redacted
      )
    }

    "filterKeys" in {
      oab.descendant("$.owner").filterKeys(_ == "a") <=> obj("owner" -> obj("a" -> j123))
    }

    "filterKeysNot" in {
      oab.descendant("$.owner").filterKeysNot(_ == "a") <=> obj("owner" -> obj("b" -> j456))
    }

    "filterValues" in {
      oab.descendant("$.owner").filterValues(_ == j123) <=> obj("owner" -> obj("a" -> j123))
    }

    "filterValuesNot" in {
      oab.descendant("$.owner").filterValuesNot(_ == j123) <=> obj("owner" -> obj("b" -> j456))
    }

    "renameFields" in {
      oab.descendant("$.owner").renameFields("b" -> "c")             <=> obj("owner" -> obj("a" -> j123, "c" -> j456))
      oab.descendant("$.owner").renameFields("a" -> "x", "b" -> "y") <=> obj("owner" -> obj("x" -> j123, "y" -> j456))
    }

    "addIfMissing" in {
      obj("owner" -> obj()).descendant("$.owner").addIfMissing("a" -> jAdded) <=> obj("owner" -> obj("a" -> jAdded))
      obj()
      obj("a" -> jExisting).addIfMissing("a" -> jAdded) <=> obj("a" -> jExisting)

      on(
        thing(obj()),         thing(obj("a" -> jExisting)),
        thing(obj("b" -> jExisting)), thing(obj("a" -> jExisting, "b" -> jExisting))
      ).calling(_.descendant("$.thing").addIfMissing("a" -> jAdded, "b" -> jAdded)).produces(
        thing(obj("a" -> jAdded, "b" -> jAdded)),    thing(obj("a" -> jExisting, "b" -> jAdded)),
        thing(obj("a" -> jAdded, "b" -> jExisting)), thing(obj("a" -> jExisting, "b" -> jExisting))
      )
    }

    "removeFields" in {
      oab.descendant("$.owner").removeFields("a") <=> obj("owner" -> obj("b" -> j456))
    }

    "retainFields" in {
      oab.descendant("$.owner").retainFields("a") <=> obj("owner" -> obj("a" -> j123))
    }

    "complex" in {
      jobj.descendant("$.preferences.*").bool.set(false)
          .descendant("$.address").array.string.modify("Flat B" :: _)
          .descendant("$.address[*]").string.modify(_.toUpperCase)
          .descendant("$.potatoes.*.variety").string.modify(_ ⇒ "Avalanche")
          .descendant("$.knownUnknowns.*").int.modify(_ ⇒ 42)
          .descendant("$.awkward.*").string.modify(_.toUpperCase) <=> parse("""
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

    "dynamic_complex" in {
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

    "descendant_ancestors" in {
      jobj.descendant("$.preferences.bananas").string.ancestors <=> obj(
        "$"                     -> spi.jArray(jobj.descendant("$").getAll),
        "$.preferences"         -> spi.jArray(jobj.descendant("$.preferences").getAll),
        "$.preferences.bananas" -> spi.jArray(jobj.descendant("$.preferences.bananas").getAll)
      )
    }

    "descendant_firstEmptyAt" in {
      assert(jobj.descendant("$.preferences.bananas").firstEmptyAt === None)
      assert(jobj.descendant("$.preferences.apples") .firstEmptyAt === Some("$.preferences.apples"))
      assert(jobj.descendant("$.pref.apples")        .firstEmptyAt === Some("$.pref"))
    }
  }

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

    val x: List[J] =
      json.descendant("$.people[?(@.person.name == 'Raymond' && @.person.age == 21)].address").getAll

    json.descendant("$.people[?(@.person.name == 'Raymond' && @.person.age == 21)].address").getAll <=> List(jString("Brisvegas"))
    json.descendant("$.people[?(@.person.name == 'Arnie' || @.person.age == 21)].address").getAll <=> List(jString("California"), jString("Brisvegas"))

    json.descendant("$.people[?(@.person.name == 'Raymond' && @.person.age != 21)].address").getAll <=> List(jString("London"))
  }

  "inequalities" in {
    val List(bob,     jim,   sue)    = List("Bob", "Jim", "Sue").map(jString(_))
    val List(pancake, round, normal) = List("pancake", "round", "normal").map(jString(_))

    obj(
      "people" -> jArray(List(
        obj("name" -> bob, "width" -> j456, "height" -> j123),
        obj("name" -> jim, "width" -> j123, "height" -> j123),
        obj("name" -> sue, "width" -> j123, "height" -> j456)
      ))
    ).descendant("$.people[?(@.width > @.height)]") .addIfMissing("description" -> pancake)
     .descendant("$.people[?(@.width == @.height)]").addIfMissing("description" -> round)
     .descendant("$.people[?(@.width < @.height)]") .addIfMissing("description" -> normal) <=> obj(
      "people" -> jArray(List(
        obj("name" -> bob, "width" -> j456, "height" -> j123, "description" -> pancake),
        obj("name" -> jim, "width" -> j123, "height" -> j123, "description" -> round),
        obj("name" -> sue, "width" -> j123, "height" -> j456, "description" -> normal)
      ))
    )
  }






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
    json.descendant("$.points[4].x").getAll <=> List(jLong(0))
    json.descendant("$.points[?(@['id']=='i4')].x").getAll <=> List(jLong(-6))
    json.descendant("$.points[*].x").getAll <=> List(4, -2, 8, -6, 0, 1).map(jLong(_))
    // Non supported syntax "$['points'][?(@['x']*@['x']+@['y']*@['y'] > 50)].id"
    json.descendant("$['points'][?(@['y'] >= 3)].id").getAll <=> List(jString("i3"), jString("i6"))
    json.descendant("$.points[?(@['z'])].id").getAll <=> List(jString("i2"), jString("i5"))
    json.descendant("$.points[?(@.z)].id").getAll <=> List(jString("i2"), jString("i5"))
    // Non supported syntax "$.points[(count(@)-1)].id"
  }

  "Multi-fields accessors should be interpreted correctly" in {
    val json = parse("""{"menu":{"year":2013,"file":"open","options":[{"bold":true},{"font":"helvetica"},{"size":3}]}}""")
    json.descendant("$.menu['file','year']").getAll <=> List(jLong(2013), jString("open"))
    json.descendant("$.menu.options['foo','bar']").getAll <=> Nil
    json.descendant("$.menu.options[*]['bold','size']").getAll <=> List(jTrue, jLong(3))
//    json.descendant("$..options['foo','bar']").getAll <=> Nil
//    json.descendant("$..options[*]['bold','size']").getAll <=> List(jTrue, jLong(3))
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

  "delete" in {
    //    println(parse(
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

    jArray(conditions.descendant("$.conditions[?(@['condition'] == true)].id").getAll)  <=> parse("""["i1"]""")
    jArray(conditions.descendant("$.conditions[?(@['condition'] == false)].id").getAll) <=> parse("""["i2"]""")

    conditions.descendant("$.conditions[?(@['condition'] == true)]").modify(_.addIfMissing("matched" -> jTrue)) <=> parse("""{
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

    jArray(objConditions.descendant("$.conditions[?(@['condition'] == true)].id").getAll)  <=> parse("""["i1"]""")
    jArray(objConditions.descendant("$.conditions[?(@['condition'] == false)].id").getAll) <=> parse("""["i2"]""")

    objConditions.descendant("$.conditions[?(@['condition'] == true)]").modify(_.addIfMissing("matched" -> jTrue)) <=> parse("""{
      "conditions": {
        "first": { "id": "i1", "condition": true, "matched": true },
        "second": { "id": "i2", "condition": false }
      }
    }""")
  }
}

abstract class JsonUtil[J: SPI] extends FreeSpecLike with Matchers {
  val spi: SPI[J] = SPI[J]

  implicit val jOrdering: Ordering[J] = spi.ordering

  import spi._

  val jTrue: J = jBoolean(true)
  val jFalse: J = jBoolean(false)

  case class on[A](as: A*) {
    case class calling[B](fs: (A ⇒ B)*) {
      def produces(bs: B*): Unit = (for {f ← fs; a ← as} yield f(a)).toList === bs.toList
    }
  }

  implicit class StringSpecSyntax(val self: String) {
    def :=[A](a: A)(implicit C: SPI.Codec[A, J]): (String, J) = self -> C.encode(a)
  }

  implicit class JsonSpecSyntax(val self: J) {
    def ->:(assoc: (String, J)): J = append(self, assoc)
    def <=>(expected: J): Unit = assertJsonEquals(self, expected)
  }

  implicit class OtherListAsserterSyntax[A](val self: List[A]) {
    def <=>(expected: List[A]): Unit = assert(self === expected)
  }

  implicit class AnySpecSyntax[A](val self: A) {
    def shouldRoundTripTo(json: J)(implicit C: SPI.Codec[A, J]): Unit = {
      C.encode(self) <=> json
      C.decode(json) shouldBe Right(self)
    }
  }

  protected def append(to: J, assoc: (String, J)): J

  protected def assertJsonEquals(actual: J, expected: J): Unit = {
    val diff = delta(actual, expected)

    if (diff != emptyObj) {
      fail(s"Detected the following differences:\n  ${pretty(diff)}")
    }
  }

  protected def delta(actual: J, expected: J): J = {
    if (actual == expected) obj() else obj(
      "actual"   := actual,
      "expected" := expected
    )
  }

  protected def pretty(json: J): String

  def reverse(json: J): J =
    jArray.modify(_.reverse).apply(json)

  val emptyObj = obj()
  val list = List("food", "foo", "bard", "bar")
  val json: J = jArray(list.map(jString(_)))

  def jsonMap(kvs: (String, String)*): J =
    obj(kvs.map { case (k, v) ⇒ (k, jString(v)) }: _*)


  val acaciaRoad = jArray(List(jString("29 Acacia Road"), jString("Nuttytown")))
  val bananas = obj("bananas" -> jTrue)
  val intObj = obj("1" -> jString("one"))

  val fields@List(lying, name, address, age, width, preferences, potatoes, knownUnknowns, awkward) = List(
    jTrue, jString("Eric"), acaciaRoad, jLong(3), jDouble(33.5), bananas,
    jArray(Nil), obj(), intObj
  )

  val jobj: J = obj(
    "name" → name, "age" → age, "lying" → lying, "address" → address, "preferences" → preferences, "width" → width,
    "potatoes" → potatoes, "knownUnknowns" → knownUnknowns, "awkward" → awkward
  )

  protected def thing(value: J): J = obj("thing" → value)

  protected val added:     String = "added"
  protected val existing:  String = "existing"
  protected val jAdded:    J   = jString(added)
  protected val jExisting: J   = jString(existing)

  val redacted: J = jString("redacted")

  def parse(jsonText: String): J

  def obj(socks: (String, J)*): J

  def reverseEntry(key: String, value: String): (String, String) = (key.reverse, value.reverse)

  final def print(j: J): Unit = println(pretty(j))
  final def print(values: List[J]): Unit = values.foreach(print)
//  def print(values: List[Json]): Unit = values.foreach(j ⇒ println(j.spaces2))
}

case class Bananaman(
  name: String, lying: Boolean, age: Int, preferences: Map[String, Boolean], address: Address,
  width: Double, knownUnknowns: Map[String, String], potatoes: List[String], awkward: Map[String, String]
)

case class Address(lines: List[String]) {
  def reverse: Address = copy(lines.reverse)
}
