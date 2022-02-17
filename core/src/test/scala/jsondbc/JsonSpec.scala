package jsondbc

import jsondbc.SPI.Codec
import jsondbc.syntax._
import org.scalatest.{FreeSpecLike, Matchers}


abstract class JsonSpec[J: SPI] extends FreeSpecLike with Matchers {
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

  implicit class JsonSpecSyntax(val self: J) {
    def →:(assoc: (String, J)): J = append(self, assoc)
    def <=>(expected: J): Unit = assertJsonEquals(self, expected)
  }

  implicit class OtherListAsserterSyntax[A](val self: List[A]) {
    def <=>(expected: List[A]): Unit = assert(self === expected)
  }

  implicit class AnySpecSyntax[A](val self: A) {
    def <=>(expected: A)(implicit codec: Codec[A, J]): Unit = assertJsonEquals(codec.encode(self), codec.encode(expected))

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
  val bananas = obj("bananas" → jTrue)
  val intObj = obj("1" → jString("one"))

  val fields@List(lying, name, address, age, width, preferences, potatoes, knownUnknowns, awkward) = List(
    jTrue, jString("Eric"), acaciaRoad, jInt(3), jDouble(33.5), bananas,
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
