package jsondbc

import io.circe.Json
import io.circe.jawn.JawnParser
import jsondbc.CirceSPI._


class CirceJsonSpec extends AbstractJsonSpec[Json] {
  protected def append(to: Json, assoc: (String, Json)): Json = to.mapObject(obj => assoc +: obj)

  def parse(jsonText: String): Json = parser.parse(jsonText) match {
    case Left(failure) => sys.error(failure.message)
    case Right(json)   => json
  }

  def obj(socks: (String, Json)*): Json = Json.obj(socks: _*)

  protected def pretty(json: Json): String = json.spaces2

  private def parser = new JawnParser
}
