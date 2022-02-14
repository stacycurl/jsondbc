package jsondbc

import jsondbc.syntax._
import jsondbc.Json4sSPI._
import org.json4s.JsonAST.{JNothing, JObject}
import org.json4s.native.Serialization
import org.json4s.{DefaultFormats, Diff, JValue, StringInput}

class Json4SJsonSpec extends AbstractJsonSpec[JValue] {
  protected def append(to: JValue, assoc: (String, JValue)): JValue = to match {
    case JObject(objFields) => JObject(assoc :: objFields.filter { case (key, _) => key != assoc._1 })
    case other              => other
  }

  override protected def assertJsonEquals(actual: JValue, expected: JValue): Unit = {
    def sortObjectEntries(unsorted: JValue): JValue = unsorted match {
      case JObject(entries) => JObject(entries.sortBy(_._1).map { case (key, value) => key -> sortObjectEntries(value) })
      case other            => other
    }

    assert(sortObjectEntries(actual).diff(sortObjectEntries(expected)) === Diff(JNothing, JNothing, JNothing))
  }

  def parse(jsonText: String): JValue = org.json4s.native.Json(DefaultFormats).parse(StringInput(jsonText))
  def obj(socks: (String, JValue)*): JValue = JObject(socks: _*)

  override protected def delta(actual: JValue, expected: JValue): JValue = {
    val diff = Diff.diff(sortObjectEntries(actual), sortObjectEntries(expected))

    obj(
      "changed" := diff.changed,
      "added"   := diff.added,
      "deleted" := diff.deleted,
    )

    super.delta(actual, expected)
  }

  protected def pretty(json: JValue): String = Serialization.writePretty(json)(DefaultFormats)

  private def sortObjectEntries(unsorted: JValue): JValue = unsorted match {
    case JObject(entries) => JObject(entries.sortBy(_._1).map { case (key, value) => key -> sortObjectEntries(value) })
    case other            => other
  }
}



