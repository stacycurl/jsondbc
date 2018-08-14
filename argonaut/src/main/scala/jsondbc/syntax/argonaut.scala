package jsondbc.syntax

import _root_.argonaut.Json._
import _root_.argonaut.JsonObjectMonocle.{jObjectEach, jObjectFilterIndex}
import _root_.argonaut.{CodecJson, DecodeJson, DecodeResult, EncodeJson, Json, JsonMonocle, JsonNumber, JsonObject}
import jsondbc.syntax.generic._
import jsondbc.{ArgonautSPI, CanPrismFrom, Descendant}
import monocle._
import monocle.function.{Each, FilterIndex}
import scalaz.\/

import scala.language.{dynamics, higherKinds, implicitConversions}

object argonaut extends ArgonautSPI {

  implicit class DescendantFrills[From, Via, To](val self: Descendant[From, Via, To]) extends AnyVal {
    def array[That]( implicit cpf: CanPrismFrom[To, List[Json], That]): Descendant[From, Via, That] = self.composePrism(cpf.prism)
    def obj[That](   implicit cpf: CanPrismFrom[To, JsonObject, That]): Descendant[From, Via, That] = self.composePrism(cpf.prism)

    def selectDynamic(key: String)(implicit cpf: CanPrismFrom[To, JsonObject, JsonObject]): Descendant[From, Via, Json] =
      obj[JsonObject] composeTraversal FilterIndex.filterIndex(Set(key))

    def as[A: CodecJson]: As[From, Via, To, A] = As[From, Via, To, A](self)
  }

  implicit class JsonFrills(val self: Json) extends AnyVal {
    def mapValuesWithKey(f: String => Json => Json): Json = self.withObject(_.mapValuesWithKey(f))

//    def renameFields(fromTos: (String, String)*): Json = self.withObject(_.renameFields(fromTos: _*))
//
//    def addIfMissing(assocs: Json.JsonAssoc*):   Json = self.withObject(_.addIfMissing(assocs: _*))

    def removeFields(names: String*): Json = self.withObject(_.removeFields(names: _*))

//    def delete(path: String): Json = {
//      path.split("/").toList.reverse match {
//        case head :: Nil ⇒ descendant("").obj.delete(head)
//        case head :: tail ⇒ descendant(tail.reverse.mkString("/")).obj.delete(head)
//        case _ ⇒ Json.jNull
//      }
//    }

    def filterR(p: Json => Boolean): Json = if (p(self)) {
      self.withObject(_.filterR(p)).withArray(_.filterR(p))
    } else {
      jNull
    }
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

  implicit class TraversalFrills[A, B](val self: Traversal[A, B]) extends AnyVal {
    def bool[That](  implicit cpf: CanPrismFrom[B, Boolean,    That]): Traversal[A, That] = apply(cpf)
    def number[That](implicit cpf: CanPrismFrom[B, JsonNumber, That]): Traversal[A, That] = apply(cpf)
    def string[That](implicit cpf: CanPrismFrom[B, String,     That]): Traversal[A, That] = apply(cpf)
    def array[That]( implicit cpf: CanPrismFrom[B, List[Json], That]): Traversal[A, That] = apply(cpf)
    def obj[That](   implicit cpf: CanPrismFrom[B, JsonObject, That]): Traversal[A, That] = apply(cpf)

    def double[That](    implicit cpf: CanPrismFrom[B, Double,     That]): Traversal[A, That] = apply(cpf)
    def int[That](       implicit cpf: CanPrismFrom[B, Int,        That]): Traversal[A, That] = apply(cpf)
    def float[That](     implicit cpf: CanPrismFrom[B, Float,      That]): Traversal[A, That] = apply(cpf)
    def short[That](     implicit cpf: CanPrismFrom[B, Short,      That]): Traversal[A, That] = apply(cpf)
    def byte[That](      implicit cpf: CanPrismFrom[B, Byte,       That]): Traversal[A, That] = apply(cpf)
    def bigDecimal[That](implicit cpf: CanPrismFrom[B, BigDecimal, That]): Traversal[A, That] = apply(cpf)
    def bigInt[That](    implicit cpf: CanPrismFrom[B, BigInt,     That]): Traversal[A, That] = apply(cpf)

    private def apply[Elem, That](canPrismFrom: CanPrismFrom[B, Elem, That]): Traversal[A, That] =
      self composePrism canPrismFrom.prism
  }

  implicit class JsonObjectFrills(val self: JsonObject) extends AnyVal {
    def removeFields(names: String*):        JsonObject = filterKeysNot(names.toSet)
    def filterKeys(p: String => Boolean):    JsonObject = mapMap(_.filterKeys(p))
    def filterKeysNot(p: String => Boolean): JsonObject = mapMap(_.filterNot(kv ⇒ p(kv._1)))
    def filterValues(p: Json => Boolean):    JsonObject = mapMap(_.filter(kv ⇒ p(kv._2)))
    def filterValuesNot(p: Json => Boolean): JsonObject = mapMap(_.filterNot(kv ⇒ p(kv._2)))

    def mapValuesWithKey(f: String => Json => Json): JsonObject = mapMap(_.map { case (k, v) => (k, f(k)(v)) })

    def renameFields(fromTos: (String, String)*): JsonObject = fromTos.foldLeft(self) {
      case (acc, (from, to)) ⇒ acc.renameFields(from, to)
    }

    private def renameFields(from: String, to: String): JsonObject =
      self(from).fold(self)(value ⇒ (self - from) + (to, value))


    def addIfMissing(assocs: Json.JsonAssoc*): JsonObject = assocs.foldLeft(self) {
      case (acc, (name, value)) ⇒ acc.addIfMissing(name, value)
    }

    private def addIfMissing(name: String, value: Json): JsonObject =
      self(name).fold(self + (name, value))(_ ⇒ self)


    def filterR(p: Json => Boolean): JsonObject = mapMap(_.collect {
      case (k, j) if p(j) => k -> j.filterR(p)
    })

    private def mapMap(f: Map[String, Json] ⇒ Map[String, Json]): JsonObject =
      JsonObject.fromTraversableOnce(f(self.toMap))
  }

  implicit class TraversalToJsonFrills[A](val self: Traversal[A, Json]) extends AnyVal {
    def renameFields(fromTos: (String, String)*): Traversal[A, Json] =
      self composeIso Iso[Json, Json](_.renameFields(fromTos: _*))(_.renameFields(fromTos.map(_.swap): _*))

    def descendant(path: String): Traversal[A, Json] =
      jsondbc.JsonPath.traversal(self, path)
  }

  implicit class JsonArrayFrills(val self: List[Json]) extends AnyVal {
    def filterR(p: Json => Boolean): List[Json] = self.collect { case j if p(j) ⇒ j.filterR(p) }
  }

  implicit class DescendantToJsonFrills[From](self: Descendant[From, Json, Json]) {
    def removeFields(names: String*): From = self.modify(_.removeFields(names: _*))

    def mapValuesWithKey(f: String => Json => Json): From = self.modify(_.mapValuesWithKey(f))

    def each: Descendant[From, Json, Json] = self composeTraversal JsonMonocle.jDescendants
  }

  implicit class DescendantToJsonObjectFrills[From](self: Descendant[From, Json, JsonObject]) {
    def renameFields(fromTos: (String, String)*): From = self.modify(_.renameFields(fromTos: _*))

    def addIfMissing(assocs: Json.JsonAssoc*):   From = self.modify(_.addIfMissing(assocs: _*))

    def removeFields(names: String*): From = self.modify(_.removeFields(names: _*))

    def filterKeys(predicate: String => Boolean): From = self.modify(_.filterKeys(predicate))
    def filterKeysNot(predicate: String => Boolean): From = self.modify(_.filterKeysNot(predicate))
    def filterValues(predicate: Json => Boolean): From = self.modify(_.filterValues(predicate))
    def filterValuesNot(predicate: Json => Boolean): From = self.modify(_.filterValuesNot(predicate))

    def mapValuesWithKey(f: String => Json => Json): From = self.modify(_.mapValuesWithKey(f))

    def each: Descendant[From, Json, Json] = self composeTraversal Each.each

    //    def delete(key: String): From = {
//      (descendant.traversal composeLens At.at(key)).set(None).apply(descendant.from)
//    }
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