package jsondbc

import jsondbc.SpraySPI._
import spray.json.{JsObject, JsValue, JsonParser}


class SprayJsonSpec extends AbstractJsonSpec[JsValue] {
  protected def append(to: JsValue, assoc: (String, JsValue)): JsValue = to match {
    case JsObject(objFields) => JsObject(objFields + assoc)
    case other => other
  }

  def parse(jsonText: String): JsValue = JsonParser.apply(jsonText)
  def obj(socks: (String, JsValue)*): JsValue = JsObject(socks: _*)

  protected def pretty(json: JsValue): String = json.prettyPrint
}
