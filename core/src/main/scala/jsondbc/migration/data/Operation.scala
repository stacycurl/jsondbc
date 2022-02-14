package jsondbc
package migration
package data

import jsondbc.SPI.Codec
import jsondbc.migration.data.Operation.Operations
import jsondbc.syntax.StringJsonSyntax
import jsondbc.util.Extractor
import monocle.Prism


object Operation {
  implicit def operationCodec[J](implicit spi: SPI[J]): Codec[Operation[J], J] = {
    val factory = new Factory[J]

    new Codec[Operation[J], J] {
      def encode(operation: Operation[J]): J = operation match {
        case AddIfMissing(entries)      ⇒ spi.obj("addIfMissing" -> spi.obj(entries))
        case FilterKeysNot(value)       ⇒ spi.obj("remove" -> spi.arr(value.toList.sorted.map(spi.jString(_)): _*))
        case FilterKeys(value)          ⇒ spi.obj("retain" -> spi.arr(value.toList.sorted.map(spi.jString(_)): _*))
        case RemoveFields(names)        ⇒ spi.obj("removeFields" := names)
        case Rename(value)              ⇒ spi.obj("rename" -> spi.obj(value.map {
          case (from, to) ⇒ from -> spi.jString(to)
        }))
        case ReplaceWith(value)         ⇒ spi.obj("replaceWith" := value)
        case _: Reverse[J]              ⇒ spi.jString("reverse")
        case _: Trim[J]                 ⇒ spi.jString("trim")
        case _: UpperCase[J]            ⇒ spi.jString("upper-case")
        case Upsert(entries)            ⇒ spi.obj("upsert" -> spi.obj(entries))
        case operations @ Operations(_) ⇒ Operations.operationsCodec.encode(operations)
      }

      def decode(json: J): Either[String, Operation[J]] = {
        val jString:        Prism[J, String]            = spi.jString
        val jObjectEntries: Prism[J, Map[String, J]] = spi.jObjectEntries
        val jStrings:       Prism[J, List[String]]      = spi.jStrings
        val jStringEntries: Prism[J, Map[String, String]] = spi.jEntries(spi.jString)

        val head = Extractor.apply[Map[String, J], (String, J)](_.toList.headOption)

        val decodedAsOperations: Either[String, Operation[J]] =
          Operations.operationsCodec.decode(json)

        decodedAsOperations.orElse {
          val optOperation: Option[Operation[J]] = PartialFunction.condOpt(json) {
            case jObjectEntries(head(("addIfMissing", jObjectEntries(entries)))) ⇒ factory.addIfMissing(entries)
            case jObjectEntries(head(("filterKeys", jStrings(fields))))          ⇒ factory.filterKeys(fields.toSet)
            case jObjectEntries(head(("filterKeysNot", jStrings(fields))))       ⇒ factory.filterKeysNot(fields.toSet)
            case jObjectEntries(head(("rename", jStringEntries(entries))))       ⇒ factory.rename(entries)
            case jObjectEntries(head(("removeFields", jStrings(names))))         ⇒ factory.removeFields(names: _*)
            case jObjectEntries(head(("replaceWith", value)))                    ⇒ factory.replaceWith(value)
            case jString("reverse")                                              ⇒ factory.reverse
            case jString("trim")                                                 ⇒ factory.trim
            case jString("upper-case")                                           ⇒ factory.upperCase
            case jObjectEntries(head(("upsert", jObjectEntries(entries))))       ⇒ factory.upsert(entries)
          }

          optOperation.toRight("Unknown operation")
        }
      }
    }
  }

  def factory[J: SPI]: Factory[J] = new Factory[J]

  class Factory[J: SPI] {
    def apply(values: (String, Operation[J])*): Operations[J] = Operations(values.map {
      case (path, operation) ⇒ Optic.create[J](path) -> operation
    }.toList)

    def nested(values: (String, Operation[J])*): Operation[J] = {
      def deepOne(value: String, operation: Operation[J]): Operations[J] = {
        val optics = Optic.optics[J](value)

        val result = optics.reverse match {
          case Nil ⇒ Operations[J](Nil)
          case last :: revInit ⇒ revInit.foldLeft(Operations(List(last -> operation))) {
            case (acc, optic) ⇒ Operations(List(optic -> acc))
          }
        }

        result
      }

      values.toList.foldLeft(Operations[J](Nil): Operation[J]) {
        case (acc, (value, operation)) ⇒ acc + deepOne(value, operation)
      }
    }

    def rename(entries: (String, String)*): Operation[J] = Rename[J](entries.toMap)
    def rename(entries: Map[String, String]): Operation[J] = Rename[J](entries)

    def removeFields(names: String*): Operation[J] = RemoveFields[J](names.toList)

    def reverse: Operation[J] = Reverse[J](SPI[J])

    def replaceWith[A](value: A)(implicit codec: Codec[A, J]): Operation[J] = ReplaceWith(codec.encode(value))

    def trim: Operation[J] = Trim[J](SPI[J])

    def upperCase: Operation[J] = UpperCase[J](SPI[J])

    def filterKeys(fields: Set[String]): Operation[J] = FilterKeys(fields)

    def filterKeysNot(fields: Set[String]): Operation[J] = FilterKeysNot(fields)

    def addIfMissing(entries: Map[String, J]): Operation[J] = AddIfMissing(entries)

    def upsert(entries: Map[String, J]): Operation[J] = Upsert(entries)
  }

  case class Operations[J](value: List[(Optic[J], Operation[J])]) extends Operation[J](json ⇒ value.foldLeft(json) {
    case (acc, (optic, operation)) ⇒ optic.apply(acc, operation)
  }) {
    def add(rhs: Operations[J]): Operations[J] = if (value.isEmpty) rhs else rhs.value.foldLeft(this) {
      case (acc, (optic, operation)) ⇒ acc.add(optic, operation)
    }

    def add(optic: Optic[J], operation: Operation[J]): Operations[J] = {
      var added: Boolean = false

      val modifiedValue: List[(Optic[J], Operation[J])] = value.map {
        case (o, m) if o == optic ⇒ {
          added = true
          o -> (m + operation)
        }
        case om                   ⇒ om
      }

      Operations(if (added) modifiedValue else value :+ (optic, operation))
    }
  }

  implicit class EitherSyntax[L, R](private val self: Either[L, R]) {
    def orElse(alternative: Either[L, R]): Either[L, R] = self match {
      case Left(_) ⇒ alternative
      case _ ⇒ self
    }
  }

  private object Operations {
    implicit def operationsCodec[J](implicit spi: SPI[J]): Codec[Operations[J], J] = {
      implicit val singleOpticPairCodec: Codec[(Optic[J], Operation[J]), J] =
        new Codec[(Optic[J], Operation[J]), J] {
          def encode(pair: (Optic[J], Operation[J])): J = pair match {
            case (Optic(path), migration) ⇒ {
              SPI[J].obj(path -> Operation.operationCodec.encode(migration))
            }
          }

          def decode(json: J): Either[String, (Optic[J], Operation[J])] = {
            val jObjectEntries: Prism[J, Map[String, J]] = spi.jObjectEntries

            json match {
              case jObjectEntries(entries) ⇒ entries.toList match {
                case (path, value) :: Nil ⇒ {
                  Operation.operationCodec.decode(value).map(Optic.create(path) → _)
                }

                case _ ⇒ Left("Could not decode Migration")
              }
              case _ ⇒ Left("Could not decode Migration")
            }
          }
        }

      Codec.listCodec[(Optic[J], Operation[J]), J]
        .xmap[Operations[J]](Operations(_))(_.value)
    }
  }

  private case class UpperCase[J](spi: SPI[J]) extends Operation[J](spi.jString.modify(_.toUpperCase))
  private case class Trim[J](spi: SPI[J]) extends Operation(spi.jString.modify(_.trim))
  private case class Reverse[J](spi: SPI[J]) extends Operation(spi.reverse)
  private case class RemoveFields[J: SPI](names: List[String]) extends Operation[J](SPI[J].removeFields(_, names: _*))
  private case class Rename[J: SPI](renames: Map[String, String]) extends Operation[J](SPI[J].renameFields(_, renames))
  private case class ReplaceWith[J: SPI](value: J) extends Operation[J](_ ⇒ value)
  private case class FilterKeysNot[J: SPI](fields: Set[String]) extends Operation[J](SPI[J].filterKeysNot(_, fields))
  private case class FilterKeys[J: SPI](fields: Set[String]) extends Operation[J](SPI[J].filterKeys(_, fields))
  private case class Upsert[J: SPI](entries: Map[String, J]) extends Operation[J](SPI[J].upsert(_, entries))
  private case class AddIfMissing[J: SPI](entries: Map[String, J]) extends Operation[J](SPI[J].addIfMissing(_, entries))
}

sealed abstract class Operation[J](migrate: J ⇒ J) {
  def apply(json: J): J = migrate(json)

  final def +(rhs: Operation[J]): Operation[J] = (this, rhs) match {
    case (l: Operations[J], r: Operations[J]) ⇒ l.add(r)
    case (l: Operations[J], r: Operation[J])  ⇒ l.add(Optic.empty, r)
    case (l: Operation[J],  r: Operations[J]) ⇒ Operations(List(Optic.empty[J] -> l)).add(r)
    case (l: Operation[J],  r: Operation[J])  ⇒ Operations(List(Optic.empty[J] -> l, Optic.empty[J] -> r))
  }
}