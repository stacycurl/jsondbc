package jsondbc
package migration
package data

import jsondbc.SPI.Codec
import jsondbc.migration.data.Migration.{Migrations, Optic}
import monocle.{Prism, Traversal}


/**
 * This representation of migrations that is serialisable (data vs code), this limits it's expressiveness, but increases it's uses
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
 *  The code here assumes you migrations will move to whereever the data is.
 *
 *  If you can move the data to the migrations you might want to use jsondbc.migration.code instead, which is more expressive,
 *  it doesn't have a fixed number of ways of migrating that this approach has.
 */

trait Migration[Json] {
  def apply(json: Json): Json

  def +(rhs: Migration[Json]): Migration[Json] = {
    (this, rhs) match {
      case (l: Migrations[Json], r: Migrations[Json]) => l.add(r)
      case (l: Migrations[Json], r: Operation[Json])  => l.add(Optic.empty, r)
      case (l: Operation[Json],  r: Migrations[Json]) => Migrations(List(Optic.empty[Json] -> l)).add(r)
      case (l: Operation[Json],  r: Operation[Json])  => Migrations(List(Optic.empty[Json] -> l, Optic.empty[Json] -> r))
      case other ⇒ sys.error("boom !")
    }
  }
}

object Migration {
  case class Optic[Json](value: String)(traversal: Traversal[Json, Json]) {
    def apply(json: Json, migration: Migration[Json]): Json = traversal.modify(migration.apply)(json)
  }

  object Optic {
    def create[Json: SPI](value: String): Optic[Json] =
      Optic(value)(JsonPath.traversal(Traversal.id[Json], value))

    def optics[Json: SPI](value: String): List[Optic[Json]] = JsonPath.relativeAncestors(value).map {
      case (token, traversal) => Optic(token)(traversal)
    }

    implicit def opticCodec[Json: SPI](implicit stringCodec: Codec[String, Json]): Codec[Optic[Json], Json] =
      stringCodec.xmap[Optic[Json]](Optic.create)(_.value)

    def empty[Json]: Optic[Json] = Optic("$")(Traversal.id[Json])
  }

  def apply[Json: SPI](values: (String, Migration[Json])*): Migrations[Json] = Migrations(values.map {
    case (path, migration) => Optic.create[Json](path) -> migration
  }.toList)

  def nested[Json: SPI](values: (String, Migration[Json])*): Migration[Json] = {
    def deepOne(value: String, migration: Migration[Json]): Migrations[Json] = {
      val optics = Optic.optics[Json](value)

      val result = optics.reverse match {
        case Nil => Migrations[Json](Nil)
        case last :: revInit => revInit.foldLeft(Migrations(List(last -> migration))) {
          case (acc, optic) => Migrations(List(optic -> acc))
        }
      }

      result
    }

    values.toList.foldLeft(Migrations[Json](Nil): Migration[Json]) {
      case (acc, (value, migration)) => acc + deepOne(value, migration)
    }
  }

  implicit class EitherSyntax[L, R](private val self: Either[L, R]) {
    def orElse(alternative: Either[L, R]): Either[L, R] = self match {
      case Left(_) ⇒ alternative
      case _ ⇒ self
    }
  }

  implicit def migrationCodec[Json: SPI]: Codec[Migration[Json], Json] = {
    new Codec[Migration[Json], Json] {
      def encode(migration: Migration[Json]): Json = migration match {
        case migrations: Migrations[Json] ⇒ Migrations.migrationsCodec.encode(migrations)
        case operation: Operation[Json] ⇒ Operation.operationCodec.encode(operation)
      }

      def decode(json: Json): Either[String, Migration[Json]] = {
        val decodedAsMigrations =
          Migrations.migrationsCodec.decode(json): Either[String, Migration[Json]]

        decodedAsMigrations.orElse(Operation.operationCodec.decode(json))
      }
    }
  }

  case class Migrations[Json](value: List[(Optic[Json], Migration[Json])]) extends Migration[Json] {
    def add(rhs: Migrations[Json]): Migrations[Json] = if (value.isEmpty) rhs else rhs.value.foldLeft(this) {
      case (acc, (optic, migration)) => acc.add(optic, migration)
    }

    def add(optic: Optic[Json], migration: Migration[Json]): Migrations[Json] = {
      var added: Boolean = false

      val modifiedValue: List[(Optic[Json], Migration[Json])] = value.map {
        case (o, m) if o == optic => {
          added = true
          o -> (m + migration)
        }
        case om                   => om
      }

      Migrations(if (added) modifiedValue else value :+ (optic, migration))
    }

    def apply(json: Json): Json = value.foldLeft(json) {
      case (acc, (optic, migration)) => optic.apply(acc, migration)
    }
  }

  object Migrations {
    implicit def migrationsCodec[Json](implicit spi: SPI[Json]): Codec[Migrations[Json], Json] = {
      implicit val singleOpticPairCodec: Codec[(Optic[Json], Migration[Json]), Json] =
        new Codec[(Optic[Json], Migration[Json]), Json] {
          def encode(pair: (Optic[Json], Migration[Json])): Json = pair match {
            case (Optic(path), migration) ⇒ {
              SPI[Json].jObject(path -> Migration.migrationCodec.encode(migration))
            }
          }

          def decode(json: Json): Either[String, (Optic[Json], Migration[Json])] = {
            val jObjectEntries: Prism[Json, Map[String, Json]] = spi.jObjectEntries

            json match {
              case jObjectEntries(entries) ⇒ entries.toList match {
                case (path, value) :: Nil ⇒ {
                  Migration.migrationCodec.decode(value).map(Optic.create(path) → _)
                }

                case _ ⇒ Left("Could not decode Migration")
              }
              case _ ⇒ Left("Could not decode Migration")
            }
          }
        }

      Codec.listCodec[(Optic[Json], Migration[Json]), Json]
        .xmap[Migrations[Json]](Migrations(_))(_.value)
    }
  }
}



