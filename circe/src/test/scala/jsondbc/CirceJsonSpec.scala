package jsondbc

import io.circe.Json
import jsondbc.CirceSPI._


class CirceJsonSpec extends AbstractJsonSpec[Json] with CirceJsonUtil

