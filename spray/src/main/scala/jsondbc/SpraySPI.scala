package jsondbc

import scala.language.higherKinds

import jsondbc.optics.monocle.scalaz.{IsoAdapter, PrismAdapter, ScalazMonocleOptics, TraversalAdapter}
import jsondbc.optics.{JIso, JPrism, JTraversal}
import monocle.function.Each
import monocle.{Iso, PTraversal}
import scalaz.Applicative
import spray.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}


object SpraySPI extends SpraySPI
trait SpraySPI {
  implicit val spraySPI: SPI[JsValue] = new SPI[JsValue] with ScalazMonocleOptics {
    type JsonObject = JsObject
    type JsonNumber = JsNumber

    def jField(jsValue: JsValue, name: String): Option[JsValue] = jsValue match {
      case JsObject(fields) => fields.get(name)
      case _                => None
    }

    val ordering: Ordering[JsValue] = {
      implicit val recurse: Ordering[JsValue] = (x: JsValue, y: JsValue) => ordering.compare(x, y)

      Ordering.Tuple5[Option[Boolean], Option[Int], Option[Double], Option[String], Option[Iterable[(String, JsValue)]]].on[JsValue](json ⇒ {
        (asBoolean(json), asInt(json), asDouble(json), asString(json), asJsObject(json).map(_.fields.toList))
      })
    }

    val jNull: JPrism[JsValue, Unit] = prism[Unit](jsonNull = Some(()))(_ => JsNull)
    val jObject: JPrism[JsValue, JsObject] = prism[JsValue, JsObject](asJsObject)(identity)
    val jArray: JPrism[JsValue, List[JsValue]] = prism[List[JsValue]](jsonArray = arr => Some(arr.elements.toList))(JsArray(_: _*))
    val jBoolean: JPrism[JsValue, Boolean] = prism[JsValue, Boolean](asBoolean)(JsBoolean(_))
    val jNumber: JPrism[JsValue, JsNumber] = prism[JsNumber](jsonNumber = Some(_))(identity)
    val jDouble: JPrism[JsValue, Double] = prism[JsValue, Double](asDouble)(JsNumber(_))
    val jString: JPrism[JsValue, String] = prism[JsValue, String](asString)(JsString(_))
    val jBigDecimal: JPrism[JsValue, BigDecimal] = prism[BigDecimal](jsonNumber = num => Some(num.value))(JsNumber(_))
    val jBigInt: JPrism[JsValue, BigInt] = prism[BigInt](jsonNumber = num => Some(num.value.toBigInt()))(JsNumber(_))
    val jLong: JPrism[JsValue, Long] = prism[Long](jsonNumber = num => Some(num.value.toLong))(JsNumber(_))
    val jInt: JPrism[JsValue, Int] = prism[JsValue, Int](asInt)(JsNumber(_))
    val jShort: JPrism[JsValue, Short] = prism[Short](jsonNumber = num => Some(num.value.toShort))(JsNumber(_))
    val jByte: JPrism[JsValue, Byte] = prism[Byte](jsonNumber = num => Some(num.value.toByte))(JsNumber(_))

    val jObjectMap: JIso[JsObject, Map[String, JsValue]] =
      IsoAdapter(Iso[JsObject, Map[String, JsValue]](_.fields)(JsObject(_)))

    val jDescendants: JTraversal[JsValue, JsValue] = TraversalAdapter {
      new PTraversal[JsValue, JsValue, JsValue, JsValue] {
        override def modifyF[F[_]](f: JsValue => F[JsValue])(s: JsValue)(implicit F: Applicative[F]): F[JsValue] =
          jsFold(F.pure(s), _ => F.pure(s), _ => F.pure(s), _ => F.pure(s),
            arr => F.map(Each.each[List[JsValue], JsValue].modifyF(f)(arr.elements.toList))(JsArray(_: _*): JsValue),
            obj => F.map(jObjectValues.adapted.modifyF(f)(obj))(jsObject => jsObject: JsValue)
          )(s)
      }
    }

    val jObjectValues: TraversalAdapter[JsObject, JsValue] = TraversalAdapter {
      new PTraversal[JsObject, JsObject, JsValue, JsValue] {
        def modifyF[F[_]](f: JsValue => F[JsValue])(o: JsObject)(implicit F: Applicative[F]): F[JsObject] = {
          F.map(o.fields.toList.foldLeft(F.point(List[(String, JsValue)]())) {
            case (acc, (k, v)) => F.apply2(acc, f(v)) {
              case (elems, newV) => (k, newV) :: elems
            }
          })(elems => JsObject(elems.reverse: _*))
        }
      }
    }

    def filterObject(p: String => Boolean): JTraversal[JsObject, JsValue] = TraversalAdapter {
      new PTraversal[JsObject, JsObject, JsValue, JsValue] {
        import scalaz.std.list._
        import scalaz.syntax.applicative._
        import scalaz.syntax.traverse._

        def modifyF[F[_]: Applicative](f: JsValue => F[JsValue])(from: JsObject): F[JsObject] =
          Applicative[F].map(
            from.fields.toList.traverse[F, (String, JsValue)]{ case (field, json) =>
              Applicative[F].map(if (p(field)) f(json) else json.point[F])(field -> _)
            }
          )(elems => JsObject(elems.reverse: _*))
      }
    }

    private def asBoolean:  JsValue => Option[Boolean]  = opt(jsonBool = Some(_))
    private def asString:   JsValue => Option[String]   = opt(jsonString = Some(_))
    private def asInt:      JsValue => Option[Int]      = opt(jsonNumber = num => Some(num.value.toInt))
    private def asDouble:   JsValue => Option[Double]   = opt(jsonNumber = num => Some(num.value.toDouble))
    private def asJsObject: JsValue => Option[JsObject] = opt(jsonObject = Some(_))

    private def opt[A](
      jsonNull: => Option[A] = None,
      jsonBool: Boolean => Option[A] = (_: Boolean) => None,
      jsonNumber: JsNumber => Option[A] = (_: JsNumber) => None,
      jsonString: String => Option[A] = (_: String) => None,
      jsonArray: JsArray => Option[A] = (_: JsArray) => None,
      jsonObject: JsObject => Option[A] = (_: JsObject) => None
    ): JsValue => Option[A] = jsFold(jsonNull, jsonBool, jsonNumber, jsonString, jsonArray, jsonObject)

    private def prism[A](
      jsonNull: => Option[A] = None,
      jsonBool: Boolean => Option[A] = (_: Boolean) => None,
      jsonNumber: JsNumber => Option[A] = (_: JsNumber) => None,
      jsonString: String => Option[A] = (_: String) => None,
      jsonArray: JsArray => Option[A] = (_: JsArray) => None,
      jsonObject: JsObject => Option[A] = (_: JsObject) => None
    )(jsonValue: A => JsValue): PrismAdapter[JsValue, A] = prism[JsValue, A](
      jsFold(jsonNull, jsonBool, jsonNumber, jsonString, jsonArray, jsonObject)
    )(jsonValue)

    private def jsFold[A](
      jsonNull: => A,
      jsonBool: Boolean => A,
      jsonNumber: JsNumber => A,
      jsonString: String => A,
      jsonArray: JsArray => A,
      jsonObject: JsObject => A
    )(jsValue: JsValue): A = jsValue match {
      case JsNull => jsonNull
      case JsBoolean(value) => jsonBool(value)
      case num: JsNumber => jsonNumber(num)
      case JsString(value) => jsonString(value)
      case arr: JsArray => jsonArray(arr)
      case obj: JsObject => jsonObject(obj)
    }
  }
}
