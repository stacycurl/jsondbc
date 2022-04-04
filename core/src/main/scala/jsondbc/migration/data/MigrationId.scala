package jsondbc
package migration
package data

import jsondbc.SPI.Codec


case class MigrationId(name: String)

object MigrationId {
  implicit def migrationIdCodec[J: SPI]: Codec[MigrationId, J] =
    Codec[String, J].xmap[MigrationId](MigrationId(_))(_.name)

  implicit val migrationIdOrdering: Ordering[MigrationId] =
    Ordering.String.on[MigrationId](_.name)
}