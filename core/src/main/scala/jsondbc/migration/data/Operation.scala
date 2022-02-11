package jsondbc
package migration
package data

import jsondbc.SPI.Codec
import jsondbc.migration.data.Migration.{Migrations, Optic}
import jsondbc.util.Extractor
import monocle.{Prism, Traversal}


object Operation {
  implicit def operationCodec[Json](implicit spi: SPI[Json]): Codec[Operation[Json], Json] = {
    val factory = new Factory[Json]

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
        val jStringEntries: Prism[Json, Map[String, String]] = spi.jEntries(spi.jString)

        val head = Extractor.apply[Map[String, Json], (String, Json)](_.toList.headOption)

        val optOperation: Option[Operation[Json]] = PartialFunction.condOpt(json) {
          case jString("upper-case")                                           => factory.upperCase
          case jString("trim")                                                 => factory.trim
          case jString("reverse")                                              => factory.reverse
          case jObjectEntries(head(("rename", jStringEntries(entries))))       => factory.rename(entries)
          case jObjectEntries(head(("filterKeys", jStrings(fields))))          => factory.filterKeys(fields.toSet)
          case jObjectEntries(head(("filterKeysNot", jStrings(fields))))       => factory.filterKeysNot(fields.toSet)
          case jObjectEntries(head(("addIfMissing", jObjectEntries(entries)))) => factory.addIfMissing(entries)
        }

        optOperation.toRight("Unknown operation")
      }
    }
  }

  def factory[Json: SPI]: Factory[Json] = new Factory[Json]

  class Factory[Json: SPI] {
    def rename(entries: (String, String)*): Operation[Json] = Rename[Json](entries.toMap)
    def rename(entries: Map[String, String]): Operation[Json] = Rename[Json](entries)

    def reverse: Operation[Json] = Reverse[Json](SPI[Json])

    def trim: Operation[Json] = Trim[Json](SPI[Json])

    def upperCase: Operation[Json] = UpperCase[Json](SPI[Json])

    def filterKeys(fields: Set[String]): Operation[Json] = FilterKeys(fields)

    def filterKeysNot(fields: Set[String]): Operation[Json] = FilterKeysNot(fields)

    def addIfMissing(entries: Map[String, Json]): Operation[Json] = AddIfMissing(entries)
  }

  private case class UpperCase[Json](spi: SPI[Json]) extends Operation[Json](spi.jString.modify(_.toUpperCase))
  private case class Trim[Json](spi: SPI[Json]) extends Operation(spi.jString.modify(_.trim))
  private case class Reverse[Json](spi: SPI[Json]) extends Operation(spi.reverse)
  private case class Rename[Json: SPI](renames: Map[String, String]) extends Operation[Json](SPI[Json].renameFields(_, renames))
  private case class FilterKeysNot[Json: SPI](fields: Set[String]) extends Operation[Json](SPI[Json].filterKeysNot(_, fields))
  private case class FilterKeys[Json: SPI](fields: Set[String]) extends Operation[Json](SPI[Json].filterKeys(_, fields))
  private case class AddIfMissing[Json: SPI](entries: Map[String, Json]) extends Operation[Json](SPI[Json].addIfMissing(_, entries))
}

sealed abstract class Operation[Json](migrate: Json => Json) extends Migration[Json] {
  def apply(json: Json): Json = migrate(json)
}