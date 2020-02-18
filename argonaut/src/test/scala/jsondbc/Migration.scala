package jsondbc

import argonaut._
import jsondbc.syntax.argonaut._
import monocle.Traversal


sealed trait Migration {
  def apply(json: Json): Json

  def +(rhs: Migration): Migration = {
    import jsondbc.Migration._

    (this, rhs) match {
      case (l: Migrations, r: Migrations) => l.add(r)
      case (l: Migrations, r: Operation)  => l.add(Optic.empty, r)
      case (l: Operation,  r: Migrations) => Migrations(List(Optic.empty -> l)).add(r)
      case (l: Operation,  r: Operation)  => Migrations(List(Optic.empty -> l, Optic.empty -> r))
    }
  }
}

object Migration {
  case class Optic(value: String)(traversal: Traversal[Json, Json]) {
    def apply(json: Json, migration: Migration): Json = traversal.modify(migration.apply)(json)
  }

  object Optic {
    def create(value: String): Optic =
      Optic(value)(JsonPath.traversal(Traversal.id[Json], value))

    def optics(value: String): List[Optic] = JsonPath.relativeAncestors(value).map {
      case (token, traversal) => Optic(token)(traversal)
    }

    implicit val opticCodec: CodecJson[Optic] = CodecJson.derived[String].xmap[Optic](Optic.create)(_.value)

    val empty: Optic = Optic("$")(Traversal.id[Json])
  }

  def apply(values: (String, Migration)*): Migrations = Migrations(values.map {
    case (path, migration) => Optic.create(path) -> migration
  }.toList)

  def nested(values: (String, Migration)*): Migration = {
    def deepOne(value: String, migration: Migration): Migrations = {
      val optics = Optic.optics(value)

      val result = optics.reverse match {
        case Nil => Migrations(Nil)
        case last :: revInit => revInit.foldLeft(Migrations(List(last -> migration))) {
          case (acc, optic) => Migrations(List(optic -> acc))
        }
      }

      result
    }

    values.toList.foldLeft(Migrations(Nil): Migration) {
      case (acc, (value, migration)) => acc + deepOne(value, migration)
    }
  }


  implicit val migrationCodec: CodecJson[Migration] = CodecJson.derived[Migration](
    EncodeJson[Migration] {
      case migrations: Migrations => Migrations.migrationsCodec.encode(migrations)
      case operation: Operation   => Operation.operationCodec.encode(operation)
    },
    Migrations.migrationsCodec.Decoder.upcast[Migration] ||| Operation.operationCodec.Decoder.upcast[Migration]
  )

  case class Migrations(value: List[(Optic, Migration)]) extends Migration {
    override lazy val toString: String = Migrations.migrationsCodec.encode(this).spaces2

    def add(rhs: Migrations): Migrations = if (value.isEmpty) rhs else rhs.value.foldLeft(this) {
      case (acc, (optic, migration)) => acc.add(optic, migration)
    }

    def add(optic: Optic, migration: Migration): Migrations = {
      var added: Boolean = false

      val modifiedValue: List[(Optic, Migration)] = value.map {
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
    implicit val migrationsCodec: CodecJson[Migrations] = {
      implicit val migrationCodec: CodecJson[(Optic, Migration)] = CodecJson.derived[(Optic, Migration)](
        EncodeJson[(Optic, Migration)] {
          case (Optic(path), operation) => Json.obj(path -> Migration.migrationCodec.encode(operation))
        },
        DecodeJson[(Optic, Migration)](cursor => cursor.fields match {
          case Some(List(path)) => cursor.get[Migration](path).map((Optic.create(path), _))
          case _                => DecodeResult.fail("Could not decode Migration", cursor.history)
        })
      )

      CodecJson.derived[List[(Optic, Migration)]].xmap[Migrations](Migrations(_))(_.value)
    }
  }

  sealed abstract class Operation(migrate: Json => Json) extends Migration {
    def apply(json: Json): Json = migrate(json)
  }

  object Operation {
    implicit val operationCodec: CodecJson[Operation] = CodecJson.derived[Operation](
      EncodeJson[Operation] {
        case UpperCase     => Json.jString("upper-case")
        case Trim          => Json.jString("trim")
        case Reverse       => Json.jString("reverse")
        case Rename(value) => Json.obj("rename" -> Json.jObject(JsonObject.fromTraversableOnce(value.map {
          case (from, to) => from -> Json.jString(to)
        })))
        case Remove(value)         => Json.obj("remove" -> Json.array(value.toList.sorted.map(Json.jString): _*))
        case Retain(value)         => Json.obj("retain" -> Json.array(value.toList.sorted.map(Json.jString): _*))
        case AddIfMissing(entries) => Json.obj("addIfMissing" -> Json.jObject(JsonObject.fromTraversableOnce(entries)))
      },
      DecodeJson[Operation](cursor => {
        val zeroArgOperation: DecodeResult[Operation] = cursor.as[String].flatMap {
          case "upper-case" => DecodeResult.ok(UpperCase)
          case "trim"       => DecodeResult.ok(Trim)
          case "reverse"    => DecodeResult.ok(Reverse)
          case other        => DecodeResult.fail(s"Unknown operation: $other", cursor.history)
        }

        val withArgsOperation: DecodeResult[Operation] =
          Rename.decode(cursor)       |||
          Remove.decode(cursor)       |||
          Retain.decode(cursor)       |||
          AddIfMissing.decode(cursor) |||
          DecodeResult.fail(s"Unknown operation: ${cursor.focus.spaces2}", cursor.history)

        zeroArgOperation ||| withArgsOperation
      })
    )
  }

  case object UpperCase                         extends Operation(_.withString(_.toUpperCase()))
  case object Trim                              extends Operation(_.withString(_.trim))
  case object Reverse                           extends Operation(_.withString(_.reverse).withArray(_.reverse))

  case class Rename(renames: Map[String, String]) extends Operation(withObjectMap(_.map {
    case (field, value) => renames.getOrElse(field, field) -> value
  }))

  object Rename {
    def apply(entries: (String, String)*): Rename = Rename(entries.toMap)

    def decode(cursor: HCursor): DecodeResult[Operation] =
      cursor.get[Map[String, String]]("rename").map(Rename(_))
  }

  object Remove {
    def decode(cursor: HCursor): DecodeResult[Operation] =
      cursor.get[List[String]]("rename").map(fields => Remove(fields.toSet))
  }

  case class Remove(fields: Set[String]) extends Operation(withObjectMap(_.collect {
    case entry@(field, _) if !fields.contains(field) => entry
  }))

  object Retain {
    def decode(cursor: HCursor): DecodeResult[Operation] =
      cursor.get[List[String]]("retain").map(fields => Retain(fields.toSet))
  }

  case class Retain(fields: Set[String]) extends Operation(withObjectMap(_.collect {
    case entry@(field, _) if fields.contains(field) => entry
  }))

  object AddIfMissing {
    def decode(cursor: HCursor): DecodeResult[Operation] =
      cursor.get[Map[String, Json]]("addIfMissing").map(AddIfMissing(_))
  }

  case class AddIfMissing(entries: Map[String, Json]) extends Operation(withObjectMap(map => {
    entries.foldLeft(map) {
      case (acc, kv@(k, _)) => acc.get(k).fold(acc + kv)(_ ⇒ acc)
    }
  }))

  private def withObjectMap(f: Map[String, Json] => Map[String, Json]): Json => Json =
    _.withObject(obj => JsonObject.fromTraversableOnce(f(obj.toMap)))
}