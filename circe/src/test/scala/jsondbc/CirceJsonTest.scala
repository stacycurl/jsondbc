package jsondbc

import io.circe.Json

import CirceSPI._


class CirceJsonTest  extends AbstractJsonTest[Json] {
  protected def append(to: Json, assoc: (String, Json)): Json = to.mapObject(obj => assoc +: obj)

  protected def assertJsonEquals(actual: Json, expected: Json): Unit = {
    assert(actual === expected)
  }

  def parse(jsonText: String): Json = Json.fromString(jsonText)

  def obj(socks: (String, Json)*): Json = Json.obj(socks: _*)

  def print(values: List[Json]): Unit = println(values)
}
