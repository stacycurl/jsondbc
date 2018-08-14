package jsondbc.syntax

import jsondbc.SPI
import monocle.{Iso, PTraversal, Prism, Traversal}
import monocle.function.{Each, FilterIndex}
import scalaz.Applicative
import spray.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}

import scala.language.higherKinds


object SpraySPI extends SpraySPI
trait SpraySPI {
  implicit val spraySPI: SPI[JsValue] = new SPI[JsValue] {
    type JsonObject = JsObject
    type JsonNumber = JsNumber

    def jField(jsValue: JsValue, name: String): Option[JsValue] = jsValue match {
      case JsObject(fields) => fields.get(name)
      case _                => None
    }

    val ordering: Ordering[JsValue] = {
      Ordering.Tuple4[Option[Boolean], Option[Int], Option[Double], Option[String]].on[JsValue](json ⇒ {
        (asBoolean(json), asInt(json), asDouble(json), asString(json))
      })
    }

    val jNull: Prism[JsValue, Unit] = prism[Unit](jsonNull = Some(()))(_ => JsNull)
    val jObject: Prism[JsValue, JsObject] = prism[JsObject](jsonObject = Some(_))(identity)
    val jArray: Prism[JsValue, List[JsValue]] = prism[List[JsValue]](jsonArray = arr => Some(arr.elements.toList))(JsArray(_: _*))
    val jBoolean: Prism[JsValue, Boolean] = Prism[JsValue, Boolean](asBoolean)(JsBoolean(_))
    val jNumber: Prism[JsValue, JsNumber] = prism[JsNumber](jsonNumber = Some(_))(identity)
    val jDouble: Prism[JsValue, Double] = Prism[JsValue, Double](asDouble)(JsNumber(_))
    val jString: Prism[JsValue, String] = Prism[JsValue, String](asString)(JsString(_))
    val jBigDecimal: Prism[JsValue, BigDecimal] = prism[BigDecimal](jsonNumber = num => Some(num.value))(JsNumber(_))
    val jBigInt: Prism[JsValue, BigInt] = prism[BigInt](jsonNumber = num => Some(num.value.toBigInt()))(JsNumber(_))
    val jLong: Prism[JsValue, Long] = prism[Long](jsonNumber = num => Some(num.value.toLong))(JsNumber(_))
    val jInt: Prism[JsValue, Int] = Prism[JsValue, Int](asInt)(JsNumber(_))
    val jShort: Prism[JsValue, Short] = prism[Short](jsonNumber = num => Some(num.value.toShort))(JsNumber(_))
    val jByte: Prism[JsValue, Byte] = prism[Byte](jsonNumber = num => Some(num.value.toByte))(JsNumber(_))

    val jObjectMap: Iso[JsObject, Map[String, JsValue]] =
      Iso[JsObject, Map[String, JsValue]](_.fields)(JsObject(_))

    val jDescendants: Traversal[JsValue, JsValue] = new PTraversal[JsValue, JsValue, JsValue, JsValue] {
      override def modifyF[F[_]](f: JsValue => F[JsValue])(s: JsValue)(implicit F: scalaz.Applicative[F]): F[JsValue] =
        jsFold(F.pure(s), _ => F.pure(s), _ => F.pure(s), _ => F.pure(s),
          arr => F.map(Each.each[List[JsValue], JsValue].modifyF(f)(arr.elements.toList))(JsArray(_: _*): JsValue),
          obj => F.map(jObjectEach.each.modifyF(f)(obj))(jsObject => jsObject: JsValue)
        )(s)
    }

    val jObjectEach: Each[JsObject, JsValue] = new Each[JsObject, JsValue] {
      def each: Traversal[JsObject, JsValue] = new PTraversal[JsObject, JsObject, JsValue, JsValue] {
        def modifyF[F[_]](f: JsValue => F[JsValue])(o: JsObject)(implicit F: Applicative[F]): F[JsObject] = {
          F.map(o.fields.toList.foldLeft(F.point(List[(String, JsValue)]())) {
            case (acc, (k, v)) => F.apply2(acc, f(v)) {
              case (elems, newV) => (k, newV) :: elems
            }
          })(elems => JsObject(elems.reverse: _*))
        }
      }
    }

    val jObjectFilterIndex: FilterIndex[JsObject, String, JsValue] = (p: String => Boolean) => {
      new PTraversal[JsObject, JsObject, JsValue, JsValue] {
        def modifyF[F[_]](f: JsValue => F[JsValue])(o: JsObject)(implicit F: Applicative[F]): F[JsObject] = {
          F.map(o.fields.toList.foldLeft(F.point(List[(String, JsValue)]())) {
            case (acc, (k, v)) if p(k) => F.apply2(acc, f(v)) {
              case (elems, newV) => (k, newV) :: elems
            }
            case (acc, _) => acc
          })(elems => JsObject(elems.reverse: _*))
        }
      }
    }

    private def asBoolean: JsValue => Option[Boolean] = opt(jsonBool = Some(_))
    private def asString: JsValue => Option[String] = opt(jsonString = Some(_))
    private def asInt: JsValue => Option[Int] = opt(jsonNumber = num => Some(num.value.toInt))
    private def asDouble: JsValue => Option[Double] = opt(jsonNumber = num => Some(num.value.toDouble))

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
    )(jsonValue: A => JsValue): Prism[JsValue, A] = Prism[JsValue, A](
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
