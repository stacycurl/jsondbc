package jsondbc

import scala.language.{dynamics, higherKinds, implicitConversions}

import io.gatling.jsonpath.AST._
import io.gatling.{jsonpath ⇒ JP}
import jsondbc.optics.{JPrism, JTraversal}
import jsondbc.util.Extractor

import scala.annotation.tailrec


case object JsonPath {
  def traversal[Json](path: String)(implicit spi: SPI[Json]): JTraversal[Json, Json] = traversal(spi.idTraversal[Json], path)
  def ancestors[Json](path: String)(implicit spi: SPI[Json]): List[(String, JTraversal[Json, Json])] = ancestors(spi.idTraversal[Json], path)

  def traversal[A, Json](from: JTraversal[A, Json], path: String)(implicit spi: SPI[Json]): JTraversal[A, Json] = {
    tokenize(path) match {
      case Right(pathTokens) ⇒ JsonPath[A, Json].traversal(pathTokens, from)
      case Left(msg)      ⇒ sys.error(msg)
    }
  }

  def ancestors[A, Json](from: JTraversal[A, Json], path: String)(implicit spi: SPI[Json]): List[(String, JTraversal[A, Json])] = {
    new JP.Parser().compile(path) match {
      case JP.Parser.Success(pathTokens, _) ⇒ JsonPath[A, Json].ancestors(pathTokens, from)
      case JP.Parser.NoSuccess(msg, _)      ⇒ sys.error(s"Could not parse json path: $path, $msg")
    }
  }

  def relativeAncestors[A, Json](path: String)(implicit spi: SPI[Json]): List[(String, JTraversal[Json, Json])] = {
    new JP.Parser().compile(path) match {
      case JP.Parser.Success(pathTokens, _) ⇒ JsonPath[A, Json].relativeAncestors(pathTokens)
      case JP.Parser.NoSuccess(msg, _)      ⇒ sys.error(s"Could not parse json path: $path, $msg")
    }
  }

  def tokenize(path: String): Either[String, List[PathToken]] = new JP.Parser().compile(path) match {
    case JP.Parser.Success(pathTokens, _) ⇒ Right(pathTokens)
    case JP.Parser.NoSuccess(msg, _)      ⇒ Left(s"Could not parse json path: $path, $msg")
  }

  def apply[A, Json](implicit spi: SPI[Json]): JsonPath[A, Json] = new JsonPath[A, Json]
}

class JsonPath[A, Json](implicit spi: SPI[Json]) {
  def traversal(tokens: List[PathToken], start: JTraversal[A, Json]): JTraversal[A, Json] = tokens.foldLeft(start)(step)

  def ancestors(tokens: List[PathToken], start: JTraversal[A, Json]): List[(String, JTraversal[A, Json])] = {
    val traversals = tokens.scanLeft(start)(step)

    annotate(tokens, traversals).tail
  }

  def relativeAncestors(tokens: List[PathToken]): List[(String, JTraversal[Json, Json])] = {
    val traversals = tokens.map(traversal)

    val stringyTokens = tokens.map(token => {
      val str = tokenToString(token)
      if (str.startsWith("$")) str else s"$$$str"
    })

    stringyTokens.zip(traversals)
  }

  def step(acc: JTraversal[A, Json], token: PathToken): JTraversal[A, Json] = {
    acc composeTraversal traversal(token)
  }

  def traversal(token: PathToken): JTraversal[Json, Json] = token match {
    case RecursiveField(name)           ⇒ notSupported(s"RecursiveField($name)")
    case RootNode                       ⇒ idTraversal
    case AnyField                       ⇒ spi.jObject composeTraversal spi.jObjectValues
    case MultiField(names)              ⇒ spi.jObject composeTraversal spi.filterObject(names.toSet: Set[String])
    case Field(name)                    ⇒ spi.jObject composeTraversal spi.filterObject(Set(name))
    case RecursiveAnyField              ⇒ notSupported("RecursiveAnyField")
    case CurrentNode                    ⇒ idTraversal
    case FILTER_TOKEN(predicate)        ⇒ filterArrayOrObjects(predicate)
    case ArraySlice(None, None, 1)      ⇒ spi.jArray composeTraversal spi.listTraversal
    case ArraySlice(begin, end, step)   ⇒ notSupported(s"ArraySlice($begin, $end, $step)")
    case ArrayRandomAccess(indecies)    ⇒ spi.jArray composeTraversal spi.filterIndexTraversal(indecies.toSet)
    case RecursiveFilterToken(filter)   ⇒ notSupported(s"RecursiveFilterToken($filter)")
  }


  private val FILTER_TOKEN: Extractor[FilterToken, Json => Boolean] = Extractor.pf[FilterToken, Json => Boolean] {
    case ComparisonFilter(CMP_OP(op), JFN(lhs),          JFN(rhs))          ⇒ json => op(lhs(json), rhs(json))
    case BooleanFilter(op,            FILTER_TOKEN(lhs), FILTER_TOKEN(rhs)) ⇒ json => op(lhs(json), rhs(json))
    case HasFilter(SubQuery(CurrentNode :: tokens))                         ⇒ subQuery(tokens, Some(_)).andThen(_.isDefined)
  }


  private val CMP_OP: Extractor[JP.ComparisonOperator, (Option[Json], Option[Json]) => Boolean] = {
    implicit val ordering: Ordering[Option[Json]] = Ordering.Option(spi.ordering)

    Extractor.pf[JP.ComparisonOperator, (Option[Json], Option[Json]) => Boolean] {
      case cmp: JP.ComparisonWithOrderingOperator ⇒ cmp.compare[Option[Json]]
      case op: JP.ComparisonOperator              ⇒ op.apply
    }
  }

  private val JPInt: Extractor[JPNumber, Int] = Extractor.pf[JPNumber, Int] {
    case JPLong(value) if value >= Int.MinValue && value <= Int.MaxValue ⇒ value.toInt
  }

  private type JFN = Json ⇒ Option[Json]

  private val JFN: Extractor[FilterValue, JFN] = Extractor.pf[FilterValue, JFN] {
    case SubQuery(CurrentNode :: tokens) ⇒ subQuery(tokens, json ⇒ Some(json))
    case JPTrue                          ⇒ _ ⇒ Some(spi.jBoolean(true))
    case JPFalse                         ⇒ _ ⇒ Some(spi.jBoolean(false))
    case JPDouble(value)                 ⇒ _ ⇒ Some(spi.jDouble(value))
    case JPInt(value)                    ⇒ _ ⇒ Some(spi.jInt(value))
    case JPLong(value)                   ⇒ _ ⇒ Some(spi.jLong(value))
    case JPString(value)                 ⇒ _ ⇒ Some(spi.jString(value))
    case JPNull                          ⇒ _ ⇒ Some(spi.jNull(()))
    case other                           ⇒ notSupported(other)
  }

  @tailrec
  private def subQuery(tokens: List[PathToken], acc: JFN): JFN = tokens match {
    case Nil                             ⇒ acc
    case Field(name)             :: tail ⇒ subQuery(tail, acc.andThen(_.flatMap(spi.jField(_, name))))
    case FILTER_TOKEN(predicate) :: tail ⇒ subQuery(tail, acc.andThen(_.filter(predicate.apply)))
    case other                           ⇒ notSupported(other)
  }

  private def notSupported[X](x: X): Nothing = sys.error(s"$x not supported !")

  private def filterArrayOrObjects(predicate: Json => Boolean): JTraversal[Json, Json] =
    spi.jDescendants composePrism predicatePrism(predicate)

  private def annotate[B](tokens: List[PathToken], traversals: List[B]): List[(String, B)] =
    tokens.map(tokenToString).inits.map(_.mkString("")).toList.reverse.zip(traversals)

  private def predicatePrism[X](p: X => Boolean): JPrism[X, X] =
    spi.prism[X, X](json ⇒ Some(json).filter(p))(json ⇒ json)

  private def tokenToString(token: PathToken): String = token match {
    case RootNode                             ⇒ "$"
    case AnyField                             ⇒ ".*"
    case MultiField(names)                    ⇒ names.map(s => s"\'$s\'").mkString(", ")
    case Field(name)                          ⇒ s".$name"
    case CurrentNode                          ⇒ "@"
    case ComparisonFilter(op, lhs, rhs)       ⇒ s"?(${toString(lhs)} ${toString(op)} ${toString(rhs)})"
    case HasFilter(SubQuery(subTokens))       ⇒ subTokens.map(tokenToString).mkString("")
    case ArraySlice(None, None, 1)            ⇒ "[*]"
    case ArrayRandomAccess(indecies)          ⇒ indecies.mkString(", ")
    case BooleanFilter(JP.AndOperator, lhs, rhs) ⇒ s"${tokenToString(lhs)} && ${tokenToString(rhs)}"
    case BooleanFilter(JP.OrOperator, lhs, rhs)  ⇒ s"${tokenToString(lhs)} || ${tokenToString(rhs)}"
    case _                                    ⇒ ???
  }

  private def toString(op: JP.ComparisonOperator): String = op match {
    case JP.EqOperator          ⇒ "=="
    case JP.NotEqOperator       ⇒ "!="
    case JP.GreaterOrEqOperator ⇒ ">="
    case JP.LessOperator        ⇒ "<"
    case JP.LessOrEqOperator    ⇒ "<="
    case JP.GreaterOperator     ⇒ ">"
  }

  private def toString(fv: FilterValue): String = fv match {
    case JPTrue           ⇒ "true"
    case JPFalse          ⇒ "false"
    case JPLong(value)    ⇒ value.toString
    case JPString(value)  ⇒ value.map(s => s"\'$s\'").mkString(", ")
    case SubQuery(tokens) ⇒ tokens.map(tokenToString).mkString("")
    case _                ⇒ sys.error(fv.toString)
  }

  private val idTraversal: JTraversal[Json, Json] = spi.idTraversal[Json]
}