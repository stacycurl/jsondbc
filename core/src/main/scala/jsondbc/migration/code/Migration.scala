package jsondbc
package migration
package code

import scala.language.implicitConversions

import jsondbc.Descendant.DescendantViaJsonFrills
import jsondbc.migration.data.MigrationId
import jsondbc.syntax._
import jsondbc.util.ClassResolver

import scala.reflect.ClassTag


object Migration {
  def migrations[J: SPI, M <: Migration[J]: ClassTag: Ordering](prefix: String)(implicit resolver: ClassResolver): List[M] =
    klassOf[M].implementationsIn(prefix).map(resolver.resolve[M]).sorted
}

/**
 * Non-serialisable representation of migrations
 * but grants greater expressiveness
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
 *  The code here assumes you data will move to whereever the migrations are, i.e. that it's accessible, this may not be
 *  possible for some environments (prod, secure, disconnected, etc.)
 *
 *  If you can't move the data to the migrations you might be able to move the migrations to the data, but then the migrations
 *  need to be serializable, and thus cannot contain custom code. That approach is in jsondbc.migration.data
 */
abstract class Migration[J](val enabled: Boolean)(implicit protected val spi: SPI[J]) {
  def migrate(json: J): MigrationResult[J]

  def id: MigrationId =
    MigrationId(getClass.getSimpleName.stripSuffix("$"))

  def isAfter(name: String): Boolean =
    id.name > name

  override def toString: String =
    getClass.getName.stripSuffix("$")

  // Syntax

  protected def delete: MigrationResult[J] =
    MigrationResult.Delete

  protected def update(value: J): MigrationResult[J] =
    MigrationResult.Update(value)

  protected def unchanged(value: J): MigrationResult[J] =
    MigrationResult.Unchanged(value)

  protected def failed(error: String): MigrationResult[J] =
    MigrationResult.Failed(error)

  protected implicit def anyFrills[A](value: A): AnyFrills[A] =
    new AnyFrills(value)

  protected implicit def descendantViaJsonFrills[From, To](descendant: Descendant[From, J, To]): DescendantViaJsonFrills[From, J, To] =
    new DescendantViaJsonFrills(descendant)

  protected implicit def stringJsonSyntax(value: String): StringJsonSyntax =
    new StringJsonSyntax(value)
}
