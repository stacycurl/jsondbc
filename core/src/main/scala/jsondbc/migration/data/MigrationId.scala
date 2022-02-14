package jsondbc
package migration
package data

import jsondbc.SPI.Codec


case class MigrationId(name: String)

object MigrationId {
  implicit def migrationidCodec[J: SPI]: Codec[MigrationId, J] =
    Codec[String, J].xmap[MigrationId](MigrationId(_))(_.name)
}