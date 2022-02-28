package jsondbc

import scala.language.higherKinds

import jsondbc.optics.monocle.scalaz.{IsoAdapter, PrismAdapter, ScalazMonocleOptics, TraversalAdapter}
import monocle.function.Each
import monocle.{PTraversal, Prism}
import org.json4s.JsonAST._
import scalaz.Applicative

object Json4sSPI extends Json4sSPI
trait Json4sSPI {
  implicit val json4sSPI: SPI.Aux[JValue, JObject, JDecimal] = new SPI[JValue] with ScalazMonocleOptics {
    type JsonObject = JObject
    type JsonNumber = JDecimal

    def jField(json: JValue, name: String): Option[JValue] = {
      for {
        obj <- jObject.getOption(json)
        value <- obj.obj.collectFirst {
          case JField(`name`, value) => value
        }
      } yield value
    }

    val ordering: Ordering[JValue] = {
      implicit val recurse: Ordering[JValue] = (x: JValue, y: JValue) => ordering.compare(x, y)

      Ordering.Tuple5[Option[Boolean], Option[Int], Option[Double], Option[String], Option[Iterable[(String, JValue)]]].on[JValue](json ⇒ {
        (asBoolean(json), asInt(json), asDouble(json), asString(json), asJsObject(json).map(_.obj))
      })
    }

    val jNull: PrismAdapter[JValue, Unit] = prism[Unit](jsonNull = Some(()))(_ => JNull)
    val jObject: PrismAdapter[JValue, JObject] = prism[JValue, JObject](asJsObject)(identity)
    val jArray: PrismAdapter[JValue, List[JValue]] = prism[List[JValue]](jsonArray = arr => Some(arr))(JArray(_))
    val jBoolean: PrismAdapter[JValue, Boolean] = prism[JValue, Boolean](asBoolean)(JBool(_))
    val jNumber: PrismAdapter[JValue, JDecimal] = prism[JDecimal](jsonNumber = Some(_))(identity)
    val jDouble: PrismAdapter[JValue, Double] = prism[JValue, Double](asDouble)(JDouble(_))
    val jString: PrismAdapter[JValue, String] = prism[JValue, String](asString)(JString)
    val jBigDecimal: PrismAdapter[JValue, BigDecimal] = prism[BigDecimal](jsonNumber = jd => Some(jd.num))(value => JDecimal(value))
    val jBigInt: PrismAdapter[JValue, BigInt] = prism[BigInt](jsonNumber = jd => Some(jd.num.toBigInt()))(value => JDecimal(BigDecimal(value)))
    val jLong: PrismAdapter[JValue, Long] = prism[Long](jsonLong = Some(_))(JLong(_))
    val jInt: PrismAdapter[JValue, Int] = prism[JValue, Int](asInt)(value => JInt(BigInt(value)))
    val jShort: PrismAdapter[JValue, Short] = prism[Short](jsonNumber = jd => Some(jd.num.toShort))(value => JDecimal(BigDecimal(value)))
    val jByte: PrismAdapter[JValue, Byte] = prism[Byte](jsonNumber = jd => Some(jd.num.toByte))(value => JDecimal(BigDecimal(value)))

    def jObjectMap: IsoAdapter[JObject, Map[String, JValue]] =
      iso[JObject, Map[String, JValue]](_.obj.toMap)(m => JObject(m.toList))

    val jDescendants: TraversalAdapter[JValue, JValue] = TraversalAdapter {
      new PTraversal[JValue, JValue, JValue, JValue] {
        override def modifyF[F[_]](f: JValue => F[JValue])(s: JValue)(implicit F: scalaz.Applicative[F]): F[JValue] =
          jsFold(F.pure(s), _ => F.pure(s), _ => F.pure(s), _ => F.pure(s), _ => F.pure(s), _ => F.pure(s), _ => F.pure(s),
            arr => F.map(Each.each[List[JValue], JValue].modifyF(f)(arr))(JArray(_): JValue),
            obj => F.map(jObjectValues.adapted.modifyF(f)(obj))(jsObject => jsObject: JValue)
          )(s)
      }
    }

    val jObjectValues: TraversalAdapter[JObject, JValue] = TraversalAdapter {
      new PTraversal[JObject, JObject, JValue, JValue] {
        def modifyF[F[_]](f: JValue => F[JValue])(o: JObject)(implicit F: Applicative[F]): F[JObject] = {
          F.map(o.obj.foldLeft(F.point(List[(String, JValue)]())) {
            case (acc, (k, v)) => F.apply2(acc, f(v)) {
              case (elems, newV) => (k, newV) :: elems
            }
          })(elems => JObject(elems.reverse: _*))
        }
      }
    }

    def filterObject(p: String => Boolean): TraversalAdapter[JObject, JValue] = TraversalAdapter {
      new PTraversal[JObject, JObject, JValue, JValue] {
        import scalaz.std.list._
        import scalaz.syntax.applicative._
        import scalaz.syntax.traverse._

        def modifyF[F[_]: Applicative](f: JValue => F[JValue])(from: JObject): F[JObject] =
          Applicative[F].map(
            from.obj.traverse[F, (String, JValue)]{ case (field, json) =>
              Applicative[F].map(if (p(field)) f(json) else json.point[F])(field -> _)
            }
          )(elems => {
            JObject(elems.reverse: _*)
          })
      }
    }

    private def asBoolean:  JValue => Option[Boolean]  = opt(jsonBool   = Some(_))
    private def asString:   JValue => Option[String]   = opt(jsonString = Some(_))

    private def asInt:      JValue => Option[Int]      = opt(
      jsonInt  = value => Some(value.num.toInt),
      jsonLong = value => Some(value.toInt)
    )

    private def asDouble:   JValue => Option[Double]   = opt(jsonDouble = Some(_))
    private def asJsObject: JValue => Option[JObject]  = opt(jsonObject = Some(_))

    private def opt[A](
      jsonNull:                => Option[A] = None,
      jsonBool:   Boolean      => Option[A] = (_: Boolean) => None,
      jsonNumber: JDecimal     => Option[A] = (_: JDecimal) => None,
      jsonDouble: Double       => Option[A] = (_: Double) => None,
      jsonInt:    JInt         => Option[A] = (_: JInt) => None,
      jsonLong:   Long         => Option[A] = (_: Long) => None,
      jsonString: String       => Option[A] = (_: String) => None,
      jsonArray:  List[JValue] => Option[A] = (_: List[JValue]) => None,
      jsonObject: JObject      => Option[A] = (_: JObject) => None
    ): JValue => Option[A] = jsFold(jsonNull, jsonBool, jsonNumber, jsonDouble, jsonInt, jsonLong, jsonString, jsonArray, jsonObject)

    private def prism[A](
      jsonNull:                => Option[A] = None,
      jsonBool:   Boolean      => Option[A] = (_: Boolean) => None,
      jsonNumber: JDecimal     => Option[A] = (_: JDecimal) => None,
      jsonDouble: Double       => Option[A] = (_: Double) => None,
      jsonInt:    JInt         => Option[A] = (_: JInt) => None,
      jsonLong:   Long         => Option[A] = (_: Long) => None,
      jsonString: String       => Option[A] = (_: String) => None,
      jsonArray:  List[JValue] => Option[A] = (_: List[JValue]) => None,
      jsonObject: JObject      => Option[A] = (_: JObject) => None
    )(jsonValue: A => JValue): PrismAdapter[JValue, A] = PrismAdapter(Prism[JValue, A](
      jsFold(jsonNull, jsonBool, jsonNumber, jsonDouble, jsonInt, jsonLong, jsonString, jsonArray, jsonObject)
    )(jsonValue))

    private def jsFold[A](
      jsonNull:                => A,
      jsonBool:   Boolean      => A,
      jsonNumber: JDecimal     => A,
      jsonDouble: Double       => A,
      jsonInt:    JInt         => A,
      jsonLong:   Long         => A,
      jsonString: String       => A,
      jsonArray:  List[JValue] => A,
      jsonObject: JObject      => A
    )(jsValue: JValue): A = jsValue match {
      case JNothing | JNull => jsonNull
      case JBool(value)     => jsonBool(value)
      case value: JDecimal  => jsonNumber(value)
      case JDouble(value)   => jsonDouble(value)
      case JLong(value)     => jsonLong(value)
      case value: JInt      => jsonInt(value)
      case JString(value)   => jsonString(value)
      case JArray(arr)      => jsonArray(arr)
      case JSet(set)        => jsonArray(set.toList)
      case value: JObject   => jsonObject(value)
    }

  }
}
