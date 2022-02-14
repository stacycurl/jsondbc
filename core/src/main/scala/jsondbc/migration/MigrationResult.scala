package jsondbc
package migration

sealed trait MigrationResult[+J]

object MigrationResult {
  case class Update[J](value: J) extends MigrationResult[J]
  case object Delete extends MigrationResult[Nothing]
  case class Failed(error: String) extends MigrationResult[Nothing]
}