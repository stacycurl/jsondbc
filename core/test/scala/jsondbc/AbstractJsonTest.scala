package jsondbc

import jsondbc.syntax.generic._
import org.scalatest.FreeSpecLike

abstract class AbstractJsonTest[J: SPI] extends JsonUtil[J] with FreeSpecLike {
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

  "descendant" - {
    "filterKeys" in {
      obj("owner" -> ab).descendant("$.owner").filterKeys(_ == "a") <=> obj("owner" -> obj("a" -> j123))
    }

    "filterKeysNot" in {
      obj("owner" -> ab).descendant("$.owner").filterKeysNot(_ == "a") <=> obj("owner" -> obj("b" -> j456))
    }

    "filterValues" in {
      obj("owner" -> ab).descendant("$.owner").filterValues(_ == j123) <=> obj("owner" -> obj("a" -> j123))
    }

    "filterValuesNot" in {
      obj("owner" -> ab).descendant("$.owner").filterValuesNot(_ == j123) <=> obj("owner" -> obj("b" -> j456))
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



  "descendant_values" in {
    jobj.descendant("$.age").getAll <=> List(age)
    jobj.descendant("$.age").modify(_ ⇒ redacted) <=> ("age" → redacted) ->: jobj

    jobj.descendant("$.name", "$.age").getAll <=> List(name, age)
    jobj.descendant("$.name", "$.age").modify(_ ⇒ redacted) <=> ("name" → redacted) ->: ("age" → redacted) ->: jobj

    jobj.descendant("$.age").int.getAll <=> List(3)
    jobj.descendant("$.age").int.modify(_ * 2) <=> ("age" → jLong(6)) ->: jobj
  }

  "descendant_elements" in {
    jArray(fields).descendant("$[0, 2]").getAll <=> List(lying, address)

    jArray(fields).descendant("$[0, 2]").modify(_ ⇒ redacted) <=> jArray(List(
      redacted, name, redacted, age, width, preferences, potatoes, knownUnknowns, awkward
    ))
  }

  "descendant_all" in {
    jobj.descendant("$.*").getAll <=> List(name, age, lying, address, preferences, width, potatoes, knownUnknowns, awkward)

    jobj.descendant("$.*").modify(_ ⇒ jString("redacted")) <=> obj(
      "name" → redacted, "age" → redacted, "lying" → redacted, "address" → redacted, "preferences" → redacted,
      "width" → redacted, "potatoes" → redacted, "knownUnknowns" → redacted, "awkward" → redacted
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



  protected def test(f: J ⇒ J, data: (String, String)*): Unit = data.foreach {
    case (input, expected) ⇒ f(parse(input)) <=> parse(expected)
  }
}

abstract class JsonUtil[J: SPI] extends FreeSpecLike {
  val spi: SPI[J] = SPI[J]

  import spi._

  val jTrue: J = jBoolean(true)
  val jFalse: J = jBoolean(false)

  case class on[A](as: A*) {
    case class calling[B](fs: (A ⇒ B)*) {
      def produces(bs: B*): Unit = (for {f ← fs; a ← as} yield f(a)).toList === bs.toList
    }
  }

  implicit class JsonSpecSyntax(val self: J) {
    def ->:(assoc: (String, J)): J = append(self, assoc)
    def <=>(expected: J): Unit = assertJsonEquals(self, expected)
  }

  implicit class OtherListAsserterSyntax[A](val self: List[A]) {
    def <=>(expected: List[A]): Unit = assert(self === expected)
  }

  protected def append(to: J, assoc: (String, J)): J
  protected def assertJsonEquals(actual: J, expected: J): Unit

  def reverse(json: J): J =
    jArray.modify(_.reverse).apply(json)


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

  def print(values: List[J]): Unit
//  def print(values: List[Json]): Unit = values.foreach(j ⇒ println(j.spaces2))

}
