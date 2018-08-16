package jsondbc

import jsondbc.SpraySPI._
import spray.json.{JsObject, JsValue, JsonParser}


class SprayJsonTest extends AbstractJsonTest[JsValue] {
  protected def append(to: JsValue, assoc: (String, JsValue)): JsValue = to match {
    case JsObject(objFields) => JsObject(objFields + assoc)
    case other => other
  }

  protected def assertJsonEquals(actual: JsValue, expected: JsValue): Unit = {
    assert(actual == expected)
  }

  def parse(jsonText: String): JsValue = JsonParser.apply(jsonText)
  def obj(socks: (String, JsValue)*): JsValue = JsObject(socks: _*)
  def print(j: JsValue): Unit = println(j.prettyPrint)
}
