package jsondbc

import jsondbc.SPI.Codec
import jsondbc.util.Extractor
import monocle.{Prism, Traversal}


sealed trait Migration[Json] {
  def apply(json: Json): Json

  def +(rhs: Migration[Json]): Migration[Json] = {
    import jsondbc.Migration._

    (this, rhs) match {
      case (l: Migrations[Json], r: Migrations[Json]) => l.add(r)
      case (l: Migrations[Json], r: Operation[Json])  => l.add(Optic.empty, r)
      case (l: Operation[Json],  r: Migrations[Json]) => Migrations(List(Optic.empty[Json] -> l)).add(r)
      case (l: Operation[Json],  r: Operation[Json])  => Migrations(List(Optic.empty[Json] -> l, Optic.empty[Json] -> r))
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


  implicit def migrationCodec[Json: SPI]: Codec[Migration[Json], Json] = {
//    CodecJson.derived[Migration](
//      EncodeJson[Migration] {
//        case migrations: Migrations => Migrations.migrationsCodec.encode(migrations)
//        case operation: Operation => Operation.operationCodec.encode(operation)
//      },
//      Migrations.migrationsCodec.Decoder.upcast[Migration] ||| Operation.operationCodec.Decoder.upcast[Migration]
//    )

    ???
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
    implicit def migrationsCodec[Json: SPI]: Codec[Migrations[Json], Json] = {
//      implicit val migrationCodec: CodecJson[(Optic, Migration)] = CodecJson.derived[(Optic, Migration)](
//        EncodeJson[(Optic, Migration)] {
//          case (Optic(path), operation) => Json.obj(path -> Migration.migrationCodec.encode(operation))
//        },
//        DecodeJson[(Optic, Migration)](cursor => cursor.fields match {
//          case Some(List(path)) => cursor.get[Migration](path).map((Optic.create(path), _))
//          case _                => DecodeResult.fail("Could not decode Migration", cursor.history)
//        })
//      )
//
//      CodecJson.derived[List[(Optic, Migration)]].xmap[Migrations](Migrations(_))(_.value)

      ???
    }
  }

  sealed abstract class Operation[Json](migrate: Json => Json) extends Migration[Json] {
    def apply(json: Json): Json = migrate(json)
  }

  object Operation {
    implicit def operationCodec[Json](implicit spi: SPI[Json]): Codec[Operation[Json], Json] = {
      new Codec[Operation[Json], Json] {
        def encode(operation: Operation[Json]): Json = operation match {
          case _: UpperCase[Json] => spi.jString("upper-case")
          case _: Trim[Json]      => spi.jString("trim")
          case _: Reverse[Json]   => spi.jString("reverse")
          case Rename(value)      => spi.jObject("rename" -> spi.jObject(value.map {
            case (from, to) => from -> spi.jString(to)
          }))
          case FilterKeysNot(value)  => spi.jObject("remove" -> spi.jArray(value.toList.sorted.map(spi.jString(_)): _*))
          case FilterKeys(value)     => spi.jObject("retain" -> spi.jArray(value.toList.sorted.map(spi.jString(_)): _*))
          case AddIfMissing(entries) => spi.jObject("addIfMissing" -> spi.jObject(entries))
        }

        def decode(json: Json): Either[String, Operation[Json]] = {
          val jString:        Prism[Json, String]            = spi.jString
          val jObjectEntries: Prism[Json, Map[String, Json]] = spi.jObjectEntries
          val jStrings:       Prism[Json, List[String]]      = spi.jStrings

          val head = Extractor.apply[Map[String, Json], (String, Json)](_.toList.headOption)

          val optOperation: Option[Operation[Json]] = PartialFunction.condOpt(json) {
            case jString("upper-case")                                           => new UpperCase[Json]
            case jString("trim")                                                 => new Trim[Json]
            case jString("reverse")                                              => new Reverse[Json]
            case jObjectEntries(head(("rename", jObjectEntries(entries))))       => Rename.create[Json](entries)
            case jObjectEntries(head(("filterKeys", jStrings(fields))))          => FilterKeys[Json](fields.toSet)
            case jObjectEntries(head(("filterKeysNot", jStrings(fields))))       => FilterKeysNot[Json](fields.toSet)
            case jObjectEntries(head(("addIfMissing", jObjectEntries(entries)))) => AddIfMissing[Json](entries)
          }

          optOperation.toRight("Unknown operation")
        }
      }
    }
  }

  class UpperCase[Json: SPI]                    extends Operation[Json](SPI[Json].jString.modify(_.toUpperCase))
  class Trim[Json: SPI]                         extends Operation(SPI[Json].jString.modify(_.trim))
  class Reverse[Json: SPI]                      extends Operation(SPI[Json].reverse)

  case class Rename[Json: SPI](renames: Map[String, String]) extends Operation[Json](SPI[Json].renameFields(_, renames))

  object Rename {
    def create[Json: SPI](entries: Map[String, Json]): Rename[Json] = ???

    def apply[Json: SPI](entries: (String, String)*): Rename[Json] = Rename[Json](entries.toMap)

//    def decode[Json](cursor: HCursor): DecodeResult[Operation] =
//      cursor.get[Map[String, String]]("rename").map(Rename(_))
  }

  object FilterKeysNot {
//    def decode[Json](cursor: HCursor): DecodeResult[Operation] =
//      cursor.get[List[String]]("rename").map(fields => FilterKeysNot(fields.toSet))
  }

  case class FilterKeysNot[Json: SPI](fields: Set[String]) extends Operation[Json](SPI[Json].filterKeysNot(_, fields))

  object FilterKeys {
//    def decode(cursor: HCursor): DecodeResult[Operation] =
//      cursor.get[List[String]]("filterKeys").map(fields => FilterKeys(fields.toSet))
  }

  case class FilterKeys[Json: SPI](fields: Set[String]) extends Operation[Json](SPI[Json].filterKeys(_, fields))

  object AddIfMissing {
//    def decode(cursor: HCursor): DecodeResult[Operation] =
//      cursor.get[Map[String, Json]]("addIfMissing").map(AddIfMissing(_))
  }

  case class AddIfMissing[Json: SPI](entries: Map[String, Json]) extends Operation[Json](SPI[Json].addIfMissing(_, entries))
}