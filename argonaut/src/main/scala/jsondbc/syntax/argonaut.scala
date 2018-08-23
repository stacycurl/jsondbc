package jsondbc.syntax

import _root_.argonaut.Json._
import _root_.argonaut.{CodecJson, DecodeJson, DecodeResult, EncodeJson, Json, JsonNumber, JsonObject}
import jsondbc.syntax.generic._
import jsondbc.{ArgonautSPI, CanPrismFrom, Descendant}
import monocle._

import scala.language.{dynamics, higherKinds, implicitConversions}
import scalaz.\/

object argonaut extends ArgonautSPI {

  implicit class DescendantFrills[From, Via, To](val self: Descendant[From, Via, To]) extends AnyVal {
    def as[A: CodecJson]: As[From, Via, To, A] = As[From, Via, To, A](self)
  }

  implicit class CodecJsonFrills[A](val self: CodecJson[A]) extends AnyVal {
    def renameFields(fromTos: (String, String)*): CodecJson[A] = afterEncode(_.renameFields(fromTos: _*))
    def addIfMissing(assocs: Json.JsonAssoc*):    CodecJson[A] = afterEncode(_.addIfMissing(assocs: _*))
    def removeFields(names: String*):             CodecJson[A] = afterEncode(_.removeFields(names: _*))

    def beforeDecode(f: Json ⇒ Json): CodecJson[A] = compose(f)
    def afterDecode(f: A ⇒ A):        CodecJson[A] = derived(encoder ⇒ encoder)(_ map f)
    def beforeEncode(f: A ⇒ A):       CodecJson[A] = derived(_ contramap f)(decoder ⇒ decoder)
    def afterEncode(f: Json ⇒ Json):  CodecJson[A] = andThen(f)
    def andThen(f: Json ⇒ Json):      CodecJson[A] = derived(_ andThen f)(decoder ⇒ decoder)
    def compose(f: Json ⇒ Json):      CodecJson[A] = derived(encoder ⇒ encoder)(_ compose f)
    def xmapDisjunction[B](f: A ⇒ String \/ B)(g: B ⇒ A): CodecJson[B] = derived(_ beforeEncode g)(_ afterDecode f)

    private[argonaut] def derived[B](f: EncodeJson[A] ⇒ EncodeJson[B])(g: DecodeJson[A] ⇒ DecodeJson[B]) =
      CodecJson.derived[B](f(self.Encoder), g(self.Decoder))
  }

  implicit class CodecJsonMapFrills[K, V](val self: CodecJson[Map[K, V]]) extends AnyVal {
    def xmapEntries[C, W](kvcw: (K, V) ⇒ (C, W))(cwkv: (C, W) ⇒ (K, V)): CodecJson[Map[C, W]] =
      self.derived[Map[C, W]](_ contramapEntries cwkv)(_ mapEntries kvcw)

    def xmapKeys[C](kc: K ⇒ C)(ck: C ⇒ K):   CodecJson[Map[C, V]] = self.derived(_ contramapKeys ck)(_ mapKeys kc)
    def xmapValues[W](vw: V ⇒ W)(wv: W ⇒ V): CodecJson[Map[K, W]] = self.derived(_ contramapValues wv)(_ mapValues vw)
  }

  implicit class DecodeJsonFrills[A](val self: DecodeJson[A]) extends AnyVal {
    def renameFields(fromTos: (String, String)*): DecodeJson[A] = beforeDecode(_.renameFields(fromTos: _*))
    def addIfMissing(assocs: Json.JsonAssoc*):    DecodeJson[A] = beforeDecode(_.addIfMissing(assocs: _*))
    def removeFields(names: String*):             DecodeJson[A] = beforeDecode(_.removeFields(names: _*))

    def beforeDecode(f: Json ⇒ Json): DecodeJson[A] = compose(f)
    def compose(f: Json ⇒ Json):      DecodeJson[A] = DecodeJson[A](hc ⇒ self.decode(hc >-> f))
    def upcast[B >: A]:               DecodeJson[B] = self.map[B](a ⇒ a: B)

    private[argonaut] def afterDecode[B](f: A ⇒ String \/ B): DecodeJson[B] = // Probably publish later
      DecodeJson[B](c ⇒ self.decode(c).flatMap(a ⇒ DecodeResult[B](f(a).leftMap(_ → c.history).toEither)))
  }

  implicit class DecodeJsonMapFrills[K, V](val self: DecodeJson[Map[K, V]]) extends AnyVal {
    def mapEntries[C, W](f: (K, V) ⇒ (C, W)): DecodeJson[Map[C, W]] = self.map(_.map {
      case (k, v) => f(k, v)
    })

    def mapKeys[C](f: K ⇒ C):   DecodeJson[Map[C, V]] = self.map(_.map { case (k, v) => f(k) -> v })
    def mapValues[W](f: V ⇒ W): DecodeJson[Map[K, W]] = self.map(_.map { case (k, v) => k -> f(v) })
  }

  implicit class EncodeJsonFrills[A](val self: EncodeJson[A]) extends AnyVal {
    def renameFields(fromTos: (String, String)*): EncodeJson[A] = afterEncode(_.renameFields(fromTos: _*))
    def addIfMissing(assocs: Json.JsonAssoc*):    EncodeJson[A] = afterEncode(_.addIfMissing(assocs: _*))
    def removeFields(names: String*):             EncodeJson[A] = afterEncode(_.removeFields(names: _*))

    def afterEncode(f: Json ⇒ Json): EncodeJson[A] = andThen(f)
    def andThen(f: Json ⇒ Json):     EncodeJson[A] = EncodeJson[A](a ⇒ f(self.encode(a)))
    def downcast[B <: A]:            EncodeJson[B] = self.contramap[B](b ⇒ b: A)

    def add(assocsFn: (A ⇒ Json.JsonAssoc)*): EncodeJson[A] = {
      EncodeJson[A](a ⇒ self.encode(a).addIfMissing(assocsFn.map(assoc ⇒ assoc.apply(a)): _*))
    }

    private[argonaut] def beforeEncode[B](f: B ⇒ A): EncodeJson[B] = self contramap f // Probably publish later
  }

  implicit class EncodeJsonMapFrills[K, V](val self: EncodeJson[Map[K, V]]) extends AnyVal {
    def contramapEntries[C, W](f: (C, W) ⇒ (K, V)): EncodeJson[Map[C, W]] = self.contramap[Map[C, W]](_.map {
      case (k, v) => f(k, v)
    })

    def contramapKeys[C](f: C ⇒ K):   EncodeJson[Map[C, V]] = self.contramap[Map[C, V]](_.map { case (k, v) => f(k) -> v })
    def contramapValues[W](f: W ⇒ V): EncodeJson[Map[K, W]] = self.contramap[Map[K, W]](_.map { case (k, v) => k -> f(v) })
  }

  implicit class DescendantViaJsonFrills[From, To](self: Descendant[From, Json, To]) {
    def firstEmptyAt: Option[String] = ancestorsList.collectFirst {
      case (path, Nil) => path
    }

    def ancestors: Json =
      Json.jObjectAssocList(ancestorsList.map { case (k, v) => k -> Json.jArray(v) })

    private def ancestorsList: List[(String, List[Json])] =
      self.ancestorsFn().map { case (k, ancestor) => k -> ancestor.getAll(self.from) }
  }

  def filterObjectP(p: Json => Boolean): Prism[Json, Json] =
    Prism[Json, Json](json ⇒ Some(json).filter(p))(json ⇒ json)

  case class As[From, Via, To, A: CodecJson](from: Descendant[From, Via, To])

  object As {
    implicit def asToDescendant[From, Via, To, A, That](as: As[From, Via, To, A])
      (implicit cpf: CanPrismFrom[To, A, That]): Descendant[From, Via, That] = as.from.composePrism(cpf.prism)
  }
}