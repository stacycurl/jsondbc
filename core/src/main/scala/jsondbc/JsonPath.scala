package jsondbc

import io.gatling.jsonpath.AST._
import io.gatling.{jsonpath => JP}
import jsondbc.util.Extractor
import monocle.{Prism, Traversal}
import monocle.function.{Each, FilterIndex}

import scala.language.{dynamics, higherKinds, implicitConversions}


case object JsonPath {
  def traversal[Json](path: String)(implicit spi: SPI[Json]): Traversal[Json, Json] = traversal(Traversal.id[Json], path)
  def ancestors[Json](path: String)(implicit spi: SPI[Json]): List[(String, Traversal[Json, Json])] = ancestors(Traversal.id[Json], path)

  def traversal[A, Json](from: Traversal[A, Json], path: String)(implicit spi: SPI[Json]): Traversal[A, Json] = {
    tokenize(path) match {
      case Right(pathTokens) ⇒ JsonPath[A, Json].traversal(pathTokens, from)
      case Left(msg)      ⇒ sys.error(msg)
    }
  }

  def ancestors[A, Json](from: Traversal[A, Json], path: String)(implicit spi: SPI[Json]): List[(String, Traversal[A, Json])] = {
    new JP.Parser().compile(path) match {
      case JP.Parser.Success(pathTokens, _) ⇒ JsonPath[A, Json].ancestors(pathTokens, from)
      case JP.Parser.NoSuccess(msg, _)      ⇒ sys.error(s"Could not parse json path: $path, $msg")
    }
  }

  def relativeAncestors[A, Json](path: String)(implicit spi: SPI[Json]): List[(String, Traversal[Json, Json])] = {
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
  def traversal(tokens: List[PathToken], start: Traversal[A, Json]): Traversal[A, Json] = tokens.foldLeft(start)(step)

  def ancestors(tokens: List[PathToken], start: Traversal[A, Json]): List[(String, Traversal[A, Json])] = {
    val traversals = tokens.scanLeft(start)(step)

    anotate(tokens, traversals).tail
  }

  def relativeAncestors(tokens: List[PathToken]): List[(String, Traversal[Json, Json])] = {
    val traversals = tokens.map(traversal)

    val stringyTokens = tokens.map(token => {
      val str = tokenToString(token)
      if (str.startsWith("$")) str else s"$$$str"
    })

    stringyTokens.zip(traversals)
  }

  def step(acc: Traversal[A, Json], token: PathToken): Traversal[A, Json] = {
    acc composeTraversal traversal(token)
  }

  def traversal(token: PathToken): Traversal[Json, Json] = token match {
      case RecursiveField(name)           ⇒ notSupported(s"RecursiveField($name)")
      case RootNode                       ⇒ idTraversal
      case AnyField                       ⇒ spi.jObject composeTraversal spi.jObjectValues
      case MultiField(names)              ⇒ spi.jObject composeTraversal spi.filterObject(names.toSet: Set[String])
      case Field(name)                    ⇒ spi.jObject composeTraversal spi.filterObject(Set(name))
      case RecursiveAnyField              ⇒ notSupported("RecursiveAnyField")
      case CurrentNode                    ⇒ idTraversal
      case FILTER_TOKEN(predicate)        ⇒ filterArrayOrObjects(predicate)
      case ArraySlice(None, None, 1)      ⇒ spi.jArray composeTraversal Each.each(Each.listEach)
      case ArraySlice(begin, end, step)   ⇒ notSupported(s"ArraySlice($begin, $end, $step)")
      case ArrayRandomAccess(indecies)    ⇒ spi.jArray composeTraversal FilterIndex.filterIndex(indecies.toSet: Set[Int])(FilterIndex.listFilterIndex)
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

  private type JFN = Json ⇒ Option[Json]

  private val JFN: Extractor[FilterValue, JFN] = Extractor.pf[FilterValue, JFN] {
    case SubQuery(CurrentNode :: tokens) ⇒ subQuery(tokens, json ⇒ Some(json))
    case JPTrue                          ⇒ _ ⇒ Some(spi.jBoolean(true))
    case JPFalse                         ⇒ _ ⇒ Some(spi.jBoolean(false))
    case JPDouble(value)                 ⇒ _ ⇒ Some(spi.jDouble(value))
    case JPLong(value)                   ⇒ _ ⇒ Some(spi.jLong(value))
    case JPString(value)                 ⇒ _ ⇒ Some(spi.jString(value))
    case JPNull                          ⇒ _ ⇒ Some(spi.jNull(()))
    case other                           ⇒ notSupported(other)
  }

  private def subQuery(tokens: List[PathToken], acc: JFN): JFN = tokens match {
    case Nil                             ⇒ acc
    case Field(name)             :: tail ⇒ subQuery(tail, acc.andThen(_.flatMap(spi.jField(_, name))))
    case FILTER_TOKEN(predicate) :: tail ⇒ subQuery(tail, acc.andThen(_.filter(predicate.apply)))
    case other                           ⇒ notSupported(other)
  }

  private def notSupported[X](x: X): Nothing = sys.error(s"$x not supported !")

  private def filterArrayOrObjects(predicate: Json => Boolean): Traversal[Json, Json] =
    spi.jDescendants composePrism predicatePrism(predicate)

  private def anotate[B](tokens: List[PathToken], traversals: List[B]): List[(String, B)] =
    tokens.map(tokenToString).inits.map(_.mkString("")).toList.reverse.zip(traversals)

  private def predicatePrism[X](p: X => Boolean): Prism[X, X] =
    Prism[X, X](json ⇒ Some(json).filter(p))(json ⇒ json)

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

  private val idTraversal: Traversal[Json, Json] = Traversal.id[Json]
}