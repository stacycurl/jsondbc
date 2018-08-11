package jsondbc

import argonaut.{Json, JsonMonocle, JsonObject, JsonObjectMonocle}
import argonaut.Json._
import io.gatling.jsonpath.AST._
import io.gatling.jsonpath._
import jsondbc.SPI.Aux
import jsondbc.syntax.Descendant.Predicate
import jsondbc.syntax.{CanPrismFrom, Descendant}
import jsondbc.util.Extractor
import monocle._
import monocle.function.{Each, FilterIndex}
import monocle.std.list.{listEach, listFilterIndex}

import scala.language.{dynamics, higherKinds, implicitConversions}


case object JsonPath {
  import jsondbc.syntax.argonaut._

  def traversal(path: String): Traversal[Json, Json] = traversal(Traversal.id[Json], path)
  def ancestors(path: String): List[(String, Traversal[Json, Json])] = ancestors(Traversal.id[Json], path)

  def traversal[A](from: Traversal[A, Json], path: String): Traversal[A, Json] = {
    new Parser().compile(path) match {
      case Parser.Success(pathTokens, _) ⇒ new JsonPathIntegration().traversal(pathTokens, from)
      case Parser.NoSuccess(msg, _)      ⇒ sys.error(s"Could not parse json path: $path, $msg")
    }
  }

  def ancestors[A](from: Traversal[A, Json], path: String): List[(String, Traversal[A, Json])] = {
    new Parser().compile(path) match {
      case Parser.Success(pathTokens, _) ⇒ new JsonPathIntegration().ancestors(pathTokens, from)
      case Parser.NoSuccess(msg, _)      ⇒ sys.error(s"Could not parse json path: $path, $msg")
    }
  }

  private class JsonPathIntegration[A] {
    object spi {
      val jNull: Json = Json.jNull
      def jBoolean(value: Boolean): Json = Json.jBool(value)
      def jDouble(value: Double): Json = Json.jNumberOrNull(value)
      def jLong(value: Long): Json = Json.jNumber(value)
      def jString(value: String): Json = Json.jString(value)
      def jField(json: Json, name: String): Option[Json] = json.field(name)

      implicit val ordering: Ordering[Json] = {
        Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[Json](json ⇒ {
          (json.bool, json.number.flatMap(_.toInt), json.number.flatMap(_.toDouble), json.string)
        })
      }

      val jObjectPrism:       Prism[Json, JsonObject]               = JsonMonocle.jObjectPrism
      val jArrayPrism:        Prism[Json, List[Json]]               = JsonMonocle.jArrayPrism
      val objectValuesOrArrayElements: Traversal[Json, Json]        = Descendant.objectValuesOrArrayElements

      val jObjectEach:        Each[JsonObject, Json]                = JsonObjectMonocle.jObjectEach
      val jObjectFilterIndex: FilterIndex[JsonObject, String, Json] = JsonObjectMonocle.jObjectFilterIndex
    }

    def traversal(tokens: List[PathToken], start: Traversal[A, Json]): Traversal[A, Json] = tokens.foldLeft(start)(step)

    def ancestors(tokens: List[PathToken], start: Traversal[A, Json]): List[(String, Traversal[A, Json])] = {
      val traversals = tokens.scanLeft(start)(step)

      anotate(tokens, traversals).tail
    }

    private def step(acc: Traversal[A, Json], token: PathToken): Traversal[A, Json] = token match {
      case RecursiveField(name)           ⇒ notSupported(s"RecursiveField($name)")
      case RootNode                       ⇒ acc
      case AnyField                       ⇒ acc composePrism spi.jObjectPrism composeTraversal Each.each(spi.jObjectEach)
      case MultiField(names)              ⇒ acc composePrism spi.jObjectPrism composeTraversal FilterIndex.filterIndex(names.toSet: Set[String])(spi.jObjectFilterIndex)
      case Field(name)                    ⇒ acc composePrism spi.jObjectPrism composeTraversal FilterIndex.filterIndex(Set(name))(spi.jObjectFilterIndex)
      case RecursiveAnyField              ⇒ notSupported("RecursiveAnyField")
      case CurrentNode                    ⇒ acc
      case FILTER_TOKEN(predicate)        ⇒ filterArrayOrObject(predicate)(acc)
      case ArraySlice(None, None, 1)      ⇒ acc composePrism spi.jArrayPrism composeTraversal Each.each(listEach)
      case ArraySlice(begin, end, step)   ⇒ notSupported(s"ArraySlice($begin, $end, $step)")
      case ArrayRandomAccess(indecies)    ⇒ acc composePrism spi.jArrayPrism composeTraversal FilterIndex.filterIndex(indecies.toSet: Set[Int])(listFilterIndex)
      case RecursiveFilterToken(filter)   ⇒ notSupported(s"RecursiveFilterToken($filter)")
    }

    private val FILTER_TOKEN: Extractor[FilterToken, Json => Boolean] = Extractor.pf[FilterToken, Json => Boolean] {
      case ComparisonFilter(CMP_OP(op), JFN(lhs),          JFN(rhs))          ⇒ json => op(lhs(json), rhs(json))
      case BooleanFilter(op,            FILTER_TOKEN(lhs), FILTER_TOKEN(rhs)) ⇒ json => op(lhs(json), rhs(json))
      case HasFilter(SubQuery(CurrentNode :: tokens))                         ⇒ subQuery(tokens, Some(_)).andThen(_.isDefined)
    }


    private val CMP_OP: Extractor[ComparisonOperator, (Option[Json], Option[Json]) => Boolean] = {
      implicit val ordering: Ordering[Option[Json]] = Ordering.Option(spi.ordering)

      Extractor.pf[ComparisonOperator, (Option[Json], Option[Json]) => Boolean] {
        case cmp: ComparisonWithOrderingOperator ⇒ cmp.compare[Option[Json]]
        case op: ComparisonOperator              ⇒ op.apply
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
      case JPNull                          ⇒ _ ⇒ Some(spi.jNull)
      case other                           ⇒ notSupported(other)
    }

    private def subQuery(tokens: List[PathToken], acc: JFN): JFN = tokens match {
      case Nil                             ⇒ acc
      case Field(name)             :: tail ⇒ subQuery(tail, acc.andThen(_.flatMap(spi.jField(_, name))))
      case FILTER_TOKEN(predicate) :: tail ⇒ subQuery(tail, acc.andThen(_.filter(predicate.apply)))
      case other                           ⇒ notSupported(other)
    }

    private def notSupported[X](x: X): Nothing = sys.error(s"$x not supported !")

    private def filterArrayOrObject(predicate: Json => Boolean)(acc: Traversal[A, Json]): Traversal[A, Json] =
      acc composeTraversal spi.objectValuesOrArrayElements composePrism predicatePrism(predicate)

    private def anotate(tokens: List[PathToken], traversals: List[Traversal[A, Json]]): List[(String, Traversal[A, Json])] =
      tokens.map(toString).inits.map(_.mkString("")).toList.reverse.zip(traversals)

    private def predicatePrism[X](p: X => Boolean): Prism[X, X] =
      Prism[X, X](json ⇒ Some(json).filter(p))(json ⇒ json)

    private def toString(token: PathToken): String = token match {
      case RootNode                             ⇒ "$"
      case AnyField                             ⇒ ".*"
      case MultiField(names)                    ⇒ names.map(s => s"\'$s\'").mkString(", ")
      case Field(name)                          ⇒ s".$name"
      case CurrentNode                          ⇒ "@"
      case ComparisonFilter(op, lhs, rhs)       ⇒ s"?(${toString(lhs)} ${toString(op)} ${toString(rhs)})"
      case HasFilter(SubQuery(subTokens))       ⇒ subTokens.map(toString).mkString("")
      case ArraySlice(None, None, 1)            ⇒ "[*]"
      case ArrayRandomAccess(indecies)          ⇒ indecies.mkString(", ")
      case BooleanFilter(AndOperator, lhs, rhs) ⇒ s"${toString(lhs)} && ${toString(rhs)}"
      case BooleanFilter(OrOperator, lhs, rhs)  ⇒ s"${toString(lhs)} || ${toString(rhs)}"
      case _                                    ⇒ ???
    }

    private def toString(op: ComparisonOperator): String = op match {
      case EqOperator          ⇒ "=="
      case NotEqOperator       ⇒ "!="
      case GreaterOrEqOperator ⇒ ">="
      case LessOperator        ⇒ "<"
      case LessOrEqOperator    ⇒ "<="
      case GreaterOperator     ⇒ ">"
    }

    private def toString(fv: FilterValue): String = fv match {
      case JPTrue           ⇒ "true"
      case JPFalse          ⇒ "false"
      case JPLong(value)    ⇒ value.toString
      case JPString(value)  ⇒ value.map(s => s"\'$s\'").mkString(", ")
      case SubQuery(tokens) ⇒ tokens.map(toString).mkString("")
      case _                ⇒ sys.error(fv.toString)
    }
  }
}