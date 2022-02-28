package jsondbc

import scala.language.higherKinds

import jsondbc.optics.monocle.scalaz.{IsoAdapter, OptionalAdapter, PrismAdapter, TraversalAdapter}
import monocle.function.{Each, FilterIndex}
import monocle.{Iso, Optional, PTraversal, Prism, Traversal}
import play.api.libs.json._
import scalaz.Applicative

object PlayJsonSPI extends PlayJsonSPI

trait PlayJsonSPI {
  implicit val spraySPI: SPI[JsValue] = new SPI[JsValue] {
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

    val jNull: PrismAdapter[JsValue, Unit] = prism[Unit](jsonNull = Some(()))(_ => JsNull)
    val jObject: PrismAdapter[JsValue, JsObject] = prism[JsValue, JsObject](asJsObject)(identity)
    val jArray: PrismAdapter[JsValue, List[JsValue]] = prism[List[JsValue]](jsonArray = arr => Some(arr.value.toList))(JsArray(_))
    val jBoolean: PrismAdapter[JsValue, Boolean] = prism[JsValue, Boolean](asBoolean)(JsBoolean(_))
    val jNumber: PrismAdapter[JsValue, JsNumber] = prism[JsNumber](jsonNumber = Some(_))(identity)
    val jDouble: PrismAdapter[JsValue, Double] = prism[JsValue, Double](asDouble)(JsNumber(_))
    val jString: PrismAdapter[JsValue, String] = prism[JsValue, String](asString)(JsString)
    val jBigDecimal: PrismAdapter[JsValue, BigDecimal] = prism[BigDecimal](jsonNumber = num => Some(num.value))(JsNumber)
    val jBigInt: PrismAdapter[JsValue, BigInt] = prism[BigInt](jsonNumber = num => Some(num.value.toBigInt()))(bigInt => JsNumber(BigDecimal(bigInt)))
    val jLong: PrismAdapter[JsValue, Long] = prism[Long](jsonNumber = num => Some(num.value.toLong))(JsNumber(_))
    val jInt: PrismAdapter[JsValue, Int] = prism[JsValue, Int](asInt)(JsNumber(_))
    val jShort: PrismAdapter[JsValue, Short] = prism[Short](jsonNumber = num => Some(num.value.toShort))(short => JsNumber(BigDecimal(short)))
    val jByte: PrismAdapter[JsValue, Byte] = prism[Byte](jsonNumber = num => Some(num.value.toByte))(byte => JsNumber(BigDecimal(byte)))

    val jObjectMap: IsoAdapter[JsObject, Map[String, JsValue]] =
      iso[JsObject, Map[String, JsValue]](_.value.toMap)(JsObject(_))

    val jDescendants: TraversalAdapter[JsValue, JsValue] = TraversalAdapter(new Traversal[JsValue, JsValue] {
      override def modifyF[F[_]](f: JsValue => F[JsValue])(s: JsValue)(implicit F: scalaz.Applicative[F]): F[JsValue] =
        jsFold(F.pure(s), _ => F.pure(s), _ => F.pure(s), _ => F.pure(s),
          arr => F.map(Each.each[List[JsValue], JsValue].modifyF(f)(arr.value.toList))(JsArray(_): JsValue),
          obj => F.map(jObjectValues.adapted.modifyF(f)(obj))(jsObject => jsObject: JsValue)
        )(s)
    })

    val jObjectValues: TraversalAdapter[JsObject, JsValue] = TraversalAdapter(new PTraversal[JsObject, JsObject, JsValue, JsValue] {
      def modifyF[F[_]](f: JsValue => F[JsValue])(o: JsObject)(implicit F: Applicative[F]): F[JsObject] = {
        F.map(o.fields.toList.foldLeft(F.point(List[(String, JsValue)]())) {
          case (acc, (k, v)) => F.apply2(acc, f(v)) {
            case (elems, newV) => (k, newV) :: elems
          }
        })(elems => JsObject(elems.reverse))
      }
    })

    def filterObject(p: String => Boolean): TraversalAdapter[JsObject, JsValue] = TraversalAdapter {
      new PTraversal[JsObject, JsObject, JsValue, JsValue] {
        import scalaz.std.list._
        import scalaz.syntax.applicative._
        import scalaz.syntax.traverse._

        def modifyF[F[_]: Applicative](f: JsValue => F[JsValue])(from: JsObject): F[JsObject] =
          Applicative[F].map(
            from.fields.toList.traverse[F, (String, JsValue)]{ case (field, json) =>
              Applicative[F].map(if (p(field)) f(json) else json.point[F])(field -> _)
            }
          )(elems => JsObject(elems.reverse))
      }
    }

    def idTraversal[A]: TraversalAdapter[A, A] =
      TraversalAdapter(Traversal.id[A])

    def listTraversal[A]: TraversalAdapter[List[A], A] =
      TraversalAdapter(Each.each(Each.listEach))

    def filterIndexTraversal[A](p: Int ⇒ Boolean): TraversalAdapter[List[A], A] =
      TraversalAdapter(FilterIndex.filterIndex(p)(FilterIndex.listFilterIndex))

    def optional[S, A](getOption: S ⇒ Option[A])(set: A ⇒ S ⇒ S): OptionalAdapter[S, A] =
      OptionalAdapter(Optional(getOption)(set))

    def prism[S, A](getOption: S ⇒ Option[A])(reverseGet: A ⇒ S): PrismAdapter[S, A] =
      PrismAdapter(Prism(getOption)(reverseGet))

    def iso[S, A](get: S => A)(reverseGet: A => S): IsoAdapter[S, A] =
      IsoAdapter(Iso(get)(reverseGet))

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
    )(jsonValue: A => JsValue): PrismAdapter[JsValue, A] = PrismAdapter(Prism[JsValue, A](
      jsFold(jsonNull, jsonBool, jsonNumber, jsonString, jsonArray, jsonObject)
    )(jsonValue))

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