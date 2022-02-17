package jsondbc

import jsondbc.SPI.Codec
import jsondbc.migration.data.Operation
import jsondbc.syntax._
import org.scalatest.FreeSpecLike

abstract class AbstractJsonSpec[J: SPI] extends JsonSpec[J] with FreeSpecLike {
  import spi._

  val (j123, j456) = (jLong(123), jLong(456))
  val ab: J = obj("a" → j123, "b" → j456)

  "descendant_case_class" - {
    val bananaMan = Bananaman(
      "Eric", lying = true, 3, Map("bananas" → true),
      Address(List("29 Acacia Road", "Nuttytown")), 33.5, Map(), Nil, Map("1" → "one")
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
        "Eric", lying = true, 3, Map("bananas" → false),
        Address(List("FLAT B", "29 ACACIA ROAD", "NUTTYTOWN")), 33.5, Map(), Nil, Map("1" → "ONE")
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
        "Eric", lying = true, 3, Map("bananas" → false),
        Address(List("FLAT B", "29 ACACIA ROAD", "NUTTYTOWN")), 33.5, Map(), Nil, Map("1" → "ONE")
      )
    }
  }

  "as" in {
    jobj.descendant("$.address").as[Address].getAll            <=> List(Address(List("29 Acacia Road", "Nuttytown")))
    jobj.descendant("$.address").as[Address].modify(_.reverse) <=> jobj.descendant("$.address").array.modify(_.reverse)
  }

  "filterKeys" in {
    ab.filterKeys(_ == "a") <=> obj("a" → j123)
  }

  "filterKeysNot" in {
    ab.filterKeysNot(_ == "a") <=> obj("b" → j456)
  }

  "filterValues" in {
    ab.filterValues(_ == j123) <=> obj("a" → j123)
  }

  "filterValuesNot" in {
    ab.filterValuesNot(_ == j123) <=> obj("b" → j456)
  }


  "filterNulls" in {
    val jNull = spi.jNull(())

    List(
      jNull                                         → jNull,
      obj("a" → jNull, "b" → j123)                → obj("b" → j123),
      jArray(List(jString("a"), jNull))             → jArray(List(jString("a"))),
      obj("o" → jArray(List(jString("a"), jNull))) → obj("o" → jArray(List(jString("a")))),
      jArray(List(obj("a" → jNull, "b" → j123)))  → jArray(List(obj("b" → j123)))
    ).foreach {
      case (input, expected) => input.filterNulls <=> expected
    }
  }

  "filterRecursive" in {
    val jEmpty = obj()

    List(
      jEmpty                                         → jNull(()),
      obj("a" → jEmpty, "b" → j123)                → obj("b" → j123),
      jArray(List(jString("a"), jEmpty))             → jArray(List(jString("a"))),
      obj("o" → jArray(List(jString("a"), jEmpty))) → obj("o" → jArray(List(jString("a")))),
      jArray(List(obj("a" → jEmpty, "b" → j123)))  → jArray(List(obj("b" → j123)))
    ).foreach {
      case (input, expected) => input.filterRecursive(_ != jEmpty) <=> expected
    }
  }

  "renameFields" in {
    obj("original" → jTrue).renameFields("original" → "renamed") <=> obj("renamed" → jTrue)
  }

  "removeFields" in {
    ab.removeFields("a") <=> obj("b" → j456)
  }

  "retainFields" in {
    ab.retainFields("a") <=> obj("a" → j123)
  }

  "addIfMissing" in {
    obj()                .addIfMissing("a" → jAdded) <=> obj("a" → jAdded)
    obj("a" → jExisting).addIfMissing("a" → jAdded) <=> obj("a" → jExisting)
  }

  "descendant" - {
    val oab = obj("owner" → ab)

    "values" in {
      jobj.descendant("$.age").getAll <=> List(age)
      jobj.descendant("$.age").modify(_ ⇒ redacted) <=> ("age" → redacted) →: jobj

      jobj.descendant("$.name", "$.age").getAll <=> List(name, age)
      jobj.descendant("$.name", "$.age").modify(_ ⇒ redacted) <=> ("name" → redacted) →: ("age" → redacted) →: jobj

      jobj.descendant("$.age").int.getAll <=> List(3)
      jobj.descendant("$.age").int.modify(_ * 2) <=> ("age" → jInt(6)) →: jobj
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
      oab.descendant("$.owner").filterKeys(_ == "a") <=> obj("owner" → obj("a" → j123))
    }

    "filterKeysNot" in {
      oab.descendant("$.owner").filterKeysNot(_ == "a") <=> obj("owner" → obj("b" → j456))
    }

    "filterValues" in {
      oab.descendant("$.owner").filterValues(_ == j123) <=> obj("owner" → obj("a" → j123))
    }

    "filterValuesNot" in {
      oab.descendant("$.owner").filterValuesNot(_ == j123) <=> obj("owner" → obj("b" → j456))
    }

    "renameFields" in {
      oab.descendant("$.owner").renameFields("b" → "c")             <=> obj("owner" → obj("a" → j123, "c" → j456))
      oab.descendant("$.owner").renameFields("a" → "x", "b" → "y") <=> obj("owner" → obj("x" → j123, "y" → j456))
    }

    "addIfMissing" in {
      obj("owner" → obj()).descendant("$.owner").addIfMissing("a" → jAdded) <=> obj("owner" → obj("a" → jAdded))
      obj()
      obj("a" → jExisting).addIfMissing("a" → jAdded) <=> obj("a" → jExisting)

      on(
        thing(obj()),         thing(obj("a" → jExisting)),
        thing(obj("b" → jExisting)), thing(obj("a" → jExisting, "b" → jExisting))
      ).calling(_.descendant("$.thing").addIfMissing("a" → jAdded, "b" → jAdded)).produces(
        thing(obj("a" → jAdded, "b" → jAdded)),    thing(obj("a" → jExisting, "b" → jAdded)),
        thing(obj("a" → jAdded, "b" → jExisting)), thing(obj("a" → jExisting, "b" → jExisting))
      )
    }

    "removeFields" in {
      oab.descendant("$.owner").removeFields("a") <=> obj("owner" → obj("b" → j456))
    }

    "retainFields" in {
      oab.descendant("$.owner").retainFields("a") <=> obj("owner" → obj("a" → j123))
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
        "$"                     → spi.jArray(jobj.descendant("$").getAll),
        "$.preferences"         → spi.jArray(jobj.descendant("$.preferences").getAll),
        "$.preferences.bananas" → spi.jArray(jobj.descendant("$.preferences.bananas").getAll)
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
      "people" → jArray(List(
        obj("name" → bob, "width" → j456, "height" → j123),
        obj("name" → jim, "width" → j123, "height" → j123),
        obj("name" → sue, "width" → j123, "height" → j456)
      ))
    ).descendant("$.people[?(@.width > @.height)]") .addIfMissing("description" → pancake)
     .descendant("$.people[?(@.width == @.height)]").addIfMissing("description" → round)
     .descendant("$.people[?(@.width < @.height)]") .addIfMissing("description" → normal) <=> obj(
      "people" → jArray(List(
        obj("name" → bob, "width" → j456, "height" → j123, "description" → pancake),
        obj("name" → jim, "width" → j123, "height" → j123, "description" → round),
        obj("name" → sue, "width" → j123, "height" → j456, "description" → normal)
      ))
    )
  }

  private val store: J = parse(
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

  "workWithBooleanFilters" in {
    val json = parse("""{ "conditions": [true, false, true] }""")

    json.descendant("$.conditions[?(@ == true)]").getAll  <=> List(jTrue, jTrue)
    json.descendant("$.conditions[?(@ == false)]").getAll <=> List(jFalse)
    json.descendant("$.conditions[?(false == @)]").getAll <=> List(jFalse)
  }

  "`work with test set 3`" in {
    val json = parse("""{ "points": [
         { "id":"i1", "x": 4, "y":-5 },
         { "id":"i2", "x":-2, "y": 2, "z":1 },
         { "id":"i3", "x": 8, "y": 3 },
         { "id":"i4", "x":-6, "y":-1 },
         { "id":"i5", "x": 0, "y": 2, "z":1 },
         { "id":"i6", "x": 1, "y": 4 }
       ]
     }"""
    )

    json.descendant("$.points[1]").getAll <=> List(parse("""{ "id":"i2", "x":-2, "y": 2, "z":1 }"""))
    json.descendant("$.points[4].x").getAll <=> List(jInt(0))
    json.descendant("$.points[?(@['id']=='i4')].x").getAll <=> List(jInt(-6))
    json.descendant("$.points[*].x").getAll <=> List(4, -2, 8, -6, 0, 1).map(jInt(_))
    // Non supported syntax "$['points'][?(@['x']*@['x']+@['y']*@['y'] > 50)].id"
    json.descendant("$['points'][?(@['y'] >= 3)].id").getAll <=> List(jString("i3"), jString("i6"))
    json.descendant("$.points[?(@['z'])].id").getAll <=> List(jString("i2"), jString("i5"))
    json.descendant("$.points[?(@.z)].id").getAll <=> List(jString("i2"), jString("i5"))
    // Non supported syntax "$.points[(count(@)-1)].id"
  }

  "Multi-fields accessors should be interpreted correctly" in {
    val json = parse("""{"menu":{"year":2013,"file":"open","options":[{"bold":true},{"font":"helvetica"},{"size":3}]}}""")
    json.descendant("$.menu['file','year']").getAll <=> List(jInt(2013), jString("open"))
    json.descendant("$.menu.options['foo','bar']").getAll <=> Nil
    json.descendant("$.menu.options[*]['bold','size']").getAll <=> List(jTrue, jInt(3))
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

    conditions.descendant("$.conditions[?(@['condition'] == true)]").modify(_.addIfMissing("matched" → jTrue)) <=> parse("""{
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

    objConditions.descendant("$.conditions[?(@['condition'] == true)]").modify(_.addIfMissing("matched" → jTrue)) <=> parse("""{
      "conditions": {
        "first": { "id": "i1", "condition": true, "matched": true },
        "second": { "id": "i2", "condition": false }
      }
    }""")
  }

  "operations" - {
    val operations = Operation.factory[J]

    val foods = obj(
      "foods" := obj(
        "fruit"  := "banana",
        "staple" := "potato ",
        "sweet"  := "cookies",
        "snack"  := "crisps",
        "drink"  := "water"
      )
    )

    val migratedFoods = obj(
      "foods" := obj(
        "fruit" := "BANANA",
        "carbs" := "otatop",
        "sweet" := "cake"
      )
    )

    "flat" in {
      val flat = operations(
        "$.foods.fruit"  → operations.upperCase,
        "$.foods.staple" → operations.trim,
        "$.foods.staple" → operations.reverse,
        "$.foods.sweet"  → operations.replaceWith("cake"),
        "$.foods"        → operations.rename("staple" → "carbs"),
        "$.foods"        → operations.removeFields("snack", "drink")
      )

      flat shouldRoundTripTo spi.arr(
        obj("$.foods.fruit"  := "upper-case"),
        obj("$.foods.staple" := "trim"),
        obj("$.foods.staple" := "reverse"),
        obj("$.foods.sweet"  := obj("replaceWith" := "cake")),
        obj("$.foods"        := obj("rename" := obj("staple" := "carbs"))),
        obj("$.foods"        := obj("removeFields" := List("snack", "drink")))
      )

      flat.apply(foods) <=> migratedFoods
    }

    "nested" in {
      val nested = operations(
        "$" → operations(
          "$.foods" → operations(
            "$.fruit"  → operations.upperCase,
            "$.staple" → operations(
              "$" → operations.trim,
              "$" → operations.reverse
            ),
            "$.sweet" → operations.replaceWith("cake"),
            "$" → operations(
              "$" → operations.rename("staple" → "carbs"),
              "$" → operations.removeFields("snack", "drink")
            )
          )
        )
      )

      nested shouldRoundTripTo spi.arr(
        obj("$" := List(
          obj("$.foods" := List(
            obj("$.fruit"  := "upper-case"),
            obj("$.staple" := List(
              obj("$" := "trim"),
              obj("$" := "reverse")
            )),
            obj("$.sweet" := obj("replaceWith" := "cake")),
            obj("$" := List(
              obj("$" := obj("rename" := obj("staple" := "carbs"))),
              obj("$" := obj("removeFields" := List("snack", "drink"))),
            ))
          ))
        ))
      )

      nested.apply(foods) <=> migratedFoods

      operations.nested(
        "$.foods.fruit"  → operations.upperCase,
        "$.foods.staple" → operations.trim,
        "$.foods.staple" → operations.reverse,
        "$.foods.sweet"  → operations.replaceWith("cake"),
        "$.foods"        → operations.rename("staple" → "carbs"),
        "$.foods"        → operations.removeFields("snack", "drink")
      ) <=> nested
    }
  }

}





object Bananaman {
  implicit def bananamanCodec[J: SPI]: Codec[Bananaman, J] = Codec(Bananaman.apply _, Bananaman.unapply _)(
    "name", "lying", "age", "preferences", "address", "width", "knownUnknowns", "potatoes", "awkward"
  )
}

case class Bananaman(
  name: String, lying: Boolean, age: Int, preferences: Map[String, Boolean], address: Address,
  width: Double, knownUnknowns: Map[String, String], potatoes: List[String], awkward: Map[String, String]
)

object Address {
  implicit def addressCodec[J: SPI]: Codec[Address, J] =
    Codec[List[String], J].xmap[Address](Address(_))(_.lines)
}

case class Address(lines: List[String]) {
  def reverse: Address = copy(lines.reverse)
}
