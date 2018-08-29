package jsondbc

import org.json4s.JsonAST.{JNothing, JObject, JValue}
import org.json4s.{DefaultFormats, Diff, StringInput}

import Json4sSPI._

class Json4SJsonSpec extends AbstractJsonSpec[JValue] {
  protected def append(to: JValue, assoc: (String, JValue)): JValue = to match {
    case JObject(objFields) => JObject(assoc :: objFields.filter { case (key, _) => key != assoc._1 })
    case other              => other
  }

  protected def assertJsonEquals(actual: JValue, expected: JValue): Unit = {
    def sortObjectEntries(unsorted: JValue): JValue = unsorted match {
      case JObject(entries) => JObject(entries.sortBy(_._1).map { case (key, value) => key -> sortObjectEntries(value) })
      case other            => other
    }

    assert(sortObjectEntries(actual).diff(sortObjectEntries(expected)) === Diff(JNothing, JNothing, JNothing))
  }

  def parse(jsonText: String): JValue = org.json4s.native.Json(DefaultFormats).parse(StringInput(jsonText))
  def obj(socks: (String, JValue)*): JValue = JObject(socks: _*)
  def print(j: JValue): Unit = println(j)
}



