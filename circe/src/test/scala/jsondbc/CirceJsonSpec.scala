package jsondbc

import io.circe.Json
import jsondbc.migration.MigrationResult
import jsondbc.migration.code.Migration


class CirceJsonSpec extends AbstractJsonSpec[Json]()(CirceSPI.circeSPI) with CirceJsonUtil {
  "migrations" in {
    assert(
      V0001_AddAThing.migrate(Json.fromString("hello")) === MigrationResult.Update(Json.fromString("HELLO"))
    )
  }
}

object V0001_AddAThing extends Migration[Json](enabled = true)(CirceSPI.circeSPI) {
  def migrate(json: Json): MigrationResult[Json] = update(
    json.descendant("$").string.modify(_.toUpperCase)
  )
}

