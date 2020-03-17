package jsondbc

import jsondbc.PlayJsonSPI._
import play.api.libs.json.{JsObject, JsValue, Json}


class PlayJsonSpec extends AbstractJsonSpec[JsValue] {
  protected def append(to: JsValue, assoc: (String, JsValue)): JsValue = to match {
    case JsObject(objFields) => JsObject(objFields + assoc)
    case other => other
  }

  def parse(jsonText: String): JsValue = Json.parse(jsonText)
  def obj(socks: (String, JsValue)*): JsValue = JsObject(socks)

  protected def pretty(json: JsValue): String = Json.prettyPrint(json)
}
