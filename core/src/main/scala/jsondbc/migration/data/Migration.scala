package jsondbc
package migration
package data

import jsondbc.SPI.Codec


/**
 * Serialisable representation of a migration, this limits it's expressiveness, but increases it's uses
 *
 * Scenario:
 *
 * 1) You changed some case classes and/or codecs
 * 2) You want to see if deploying the code will work in some environment,
 *    i.e. will the data in that environment be decodable losslessly to the new schema
 * 3) Therefore you need to take the data in the environment, apply any new migrations to it, and then decode/encode it
 *    with the new code/codecs.
 * 4) This always requires the migrations & data be co-located.
 *
 *  The code here assumes your migrations will move to whereever the data is.
 *
 *  If you can move the data to the migrations you might want to use jsondbc.migration.code instead, which is more expressive,
 *  it doesn't have a fixed number of ways of migrating that this approach has.
 */

case class Migration[J](id: MigrationId, enabled: Boolean, operation: Operation[J]) {
  def apply(json: J): MigrationResult[J] =
    MigrationResult.Update(operation.apply(json))
}

object Migration {
  implicit def migrationCodec[J](implicit spi: SPI[J]): Codec[Migration[J], J] =
    Codec(Migration.apply[J] _, Migration.unapply[J] _)("id", "enabled", "operation")
}

