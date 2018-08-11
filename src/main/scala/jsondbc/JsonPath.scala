package jsondbc

import _root_.argonaut.Json
import _root_.argonaut.Json._
import _root_.argonaut.JsonObjectMonocle.{jObjectEach, jObjectFilterIndex}
import io.gatling.jsonpath.AST._
import io.gatling.jsonpath._
import jsondbc.syntax.Descendant
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
    def traversal(tokens: List[PathToken], start: Traversal[A, Json]): Traversal[A, Json] = tokens.foldLeft(start)(step)

    def ancestors(tokens: List[PathToken], start: Traversal[A, Json]): List[(String, Traversal[A, Json])] = {
      val traversals = tokens.scanLeft(start)(step)

      anotate(tokens, traversals).tail
    }

    private def step(acc: Traversal[A, Json], token: PathToken): Traversal[A, Json] = token match {
      case RecursiveField(name)           ⇒ notSupported(s"RecursiveField($name)")
      case RootNode                       ⇒ acc
      case AnyField                       ⇒ acc.obj composeTraversal Each.each
      case MultiField(names)              ⇒ acc.obj composeTraversal FilterIndex.filterIndex(names.toSet: Set[String])
      case Field(name)                    ⇒ acc.obj composeTraversal FilterIndex.filterIndex(Set(name))
      case RecursiveAnyField              ⇒ notSupported("RecursiveAnyField")
      case CurrentNode                    ⇒ acc
      case FILTER_TOKEN(predicate)        ⇒ filterArrayOrObject(predicate)(acc)
      case ArraySlice(None, None, 1)      ⇒ acc.array composeTraversal Each.each
      case ArraySlice(begin, end, step)   ⇒ notSupported(s"ArraySlice($begin, $end, $step)")
      case ArrayRandomAccess(indecies)    ⇒ acc.array composeTraversal FilterIndex.filterIndex(indecies.toSet: Set[Int])
      case RecursiveFilterToken(filter)   ⇒ notSupported(s"RecursiveFilterToken($filter)")
    }

    private val FILTER_TOKEN: Extractor[FilterToken, Json => Boolean] = Extractor.pf[FilterToken, Json => Boolean] {
      case ComparisonFilter(CMP_OP(op), JFN(lhs),          JFN(rhs))          ⇒ json => op(lhs(json), rhs(json))
      case BooleanFilter(op,            FILTER_TOKEN(lhs), FILTER_TOKEN(rhs)) ⇒ json => op(lhs(json), rhs(json))
      case HasFilter(SubQuery(CurrentNode :: tokens))                         ⇒ subQuery(tokens, Some(_)).andThen(_.isDefined)
    }

    private implicit val orderingJson: Ordering[Json] = {
      Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[Json](json ⇒ {
        (json.bool, json.number.flatMap(_.toInt), json.number.flatMap(_.toDouble), json.string)
      })
    }

    private val CMP_OP: Extractor[ComparisonOperator, (Option[Json], Option[Json]) => Boolean] = {
      Extractor.pf[ComparisonOperator, (Option[Json], Option[Json]) => Boolean] {
        case cmp: ComparisonWithOrderingOperator ⇒ cmp.compare[Option[Json]]
        case op: ComparisonOperator              ⇒ op.apply
      }
    }

    private type JFN = Json ⇒ Option[Json]

    private val JFN: Extractor[FilterValue, JFN] = Extractor.pf[FilterValue, JFN] {
      case SubQuery(CurrentNode :: tokens) ⇒ subQuery(tokens, json ⇒ Some(json))
      case JPTrue                          ⇒ _ ⇒ Some(jTrue)
      case JPFalse                         ⇒ _ ⇒ Some(jFalse)
      case JPDouble(value)                 ⇒ _ ⇒ Some(Json.jNumberOrNull(value))
      case JPLong(value)                   ⇒ _ ⇒ Some(Json.jNumber(value))
      case JPString(value)                 ⇒ _ ⇒ Some(jString(value))
      case JPNull                          ⇒ _ ⇒ Some(jNull)
      case other                           ⇒ notSupported(other)
    }

    private def subQuery(tokens: List[PathToken], acc: JFN): JFN = tokens match {
      case Nil                             ⇒ acc
      case Field(name)             :: tail ⇒ subQuery(tail, acc.andThen(_.flatMap(_.field(name))))
      case FILTER_TOKEN(predicate) :: tail ⇒ subQuery(tail, acc.andThen(_.filter(predicate.apply)))
      case other                           ⇒ notSupported(other)
    }

    private def notSupported[X](x: X): Nothing = sys.error(s"$x not supported !")

    private def filterArrayOrObject(predicate: Json => Boolean)(acc: Traversal[A, Json]): Traversal[A, Json] =
      acc composeTraversal Descendant.objectValuesOrArrayElements composePrism Descendant.filterObjectP(predicate)

    private def anotate(tokens: List[PathToken], traversals: List[Traversal[A, Json]]): List[(String, Traversal[A, Json])] = {
      tokens.map(toString).inits.map(_.mkString("")).toList.reverse.zip(traversals)
    }

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
