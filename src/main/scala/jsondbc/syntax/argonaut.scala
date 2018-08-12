package jsondbc.syntax

import _root_.argonaut.Json._
import _root_.argonaut.JsonObjectMonocle.{jObjectEach, jObjectFilterIndex}
import _root_.argonaut.{CodecJson, DecodeJson, DecodeResult, EncodeJson, Json, JsonMonocle, JsonNumber, JsonObject}
import monocle._
import monocle.function.{Each, FilterIndex}
import scalaz.{Applicative, \/}

import scala.collection.immutable.{Map => ▶:}
import scala.language.{dynamics, higherKinds, implicitConversions}


object argonaut {
  type Predicate[A] = A => Boolean

  implicit class JsonFrills(val self: Json) extends AnyVal {
    def descendant: Descendant[Json, Json, Json] =
      Descendant(self, List(Traversal.id[Json]), () => List("" -> Traversal.id[Json]))

    def descendant(paths: String*): Descendant[Json, Json, Json] = Descendant(self,
      paths.map(jsondbc.JsonPath.traversal[Json])(collection.breakOut),
      () => paths.flatMap(jsondbc.JsonPath.ancestors[Json])(collection.breakOut)
    )

    def compact:                             Json = filterNulls
    def filterNulls:                         Json = filterR(_ != jNull)
    def filterKeys(p: Predicate[String]):    Json = self.withObject(_.filterKeys(p))
    def filterKeysNot(p: Predicate[String]): Json = self.withObject(_.filterKeysNot(p))
    def filterValues(p: Predicate[Json]):    Json = self.withObject(_.filterValues(p))
    def filterValuesNot(p: Predicate[Json]): Json = self.withObject(_.filterValuesNot(p))

    def mapValuesWithKey(f: String => Json => Json): Json = self.withObject(_.mapValuesWithKey(f))

    def renameFields(fromTos: (String, String)*): Json = self.withObject(_.renameFields(fromTos: _*))

    def addIfMissing(assocs: Json.JsonAssoc*):   Json = self.withObject(_.addIfMissing(assocs: _*))

    def removeFields(names: String*): Json = self.withObject(_.removeFields(names: _*))

//    def delete(path: String): Json = {
//      path.split("/").toList.reverse match {
//        case head :: Nil ⇒ descendant("").obj.delete(head)
//        case head :: tail ⇒ descendant(tail.reverse.mkString("/")).obj.delete(head)
//        case _ ⇒ Json.jNull
//      }
//    }

    def filterR(p: Predicate[Json]): Json = if (p(self)) {
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

  implicit class CodecJsonMapFrills[K, V](val self: CodecJson[K ▶: V]) extends AnyVal {
    def xmapEntries[C, W](kvcw: (K, V) ⇒ (C, W))(cwkv: (C, W) ⇒ (K, V)): CodecJson[C ▶: W] =
      self.derived[C ▶: W](_ contramapEntries cwkv)(_ mapEntries kvcw)

    def xmapKeys[C](kc: K ⇒ C)(ck: C ⇒ K):   CodecJson[C ▶: V] = self.derived(_ contramapKeys ck)(_ mapKeys kc)
    def xmapValues[W](vw: V ⇒ W)(wv: W ⇒ V): CodecJson[K ▶: W] = self.derived(_ contramapValues wv)(_ mapValues vw)
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

  implicit class DecodeJsonMapFrills[K, V](val self: DecodeJson[K ▶: V]) extends AnyVal {
    def mapEntries[C, W](f: (K, V) ⇒ (C, W)): DecodeJson[C ▶: W] = self.map(_.map {
      case (k, v) => f(k, v)
    })

    def mapKeys[C](f: K ⇒ C):   DecodeJson[C ▶: V] = self.map(_.map { case (k, v) => f(k) -> v })
    def mapValues[W](f: V ⇒ W): DecodeJson[K ▶: W] = self.map(_.map { case (k, v) => k -> f(v) })
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

  implicit class EncodeJsonMapFrills[K, V](val self: EncodeJson[K ▶: V]) extends AnyVal {
    def contramapEntries[C, W](f: (C, W) ⇒ (K, V)): EncodeJson[C ▶: W] = self.contramap[C ▶: W](_.map {
      case (k, v) => f(k, v)
    })

    def contramapKeys[C](f: C ⇒ K):   EncodeJson[C ▶: V] = self.contramap[C ▶: V](_.map { case (k, v) => f(k) -> v })
    def contramapValues[W](f: W ⇒ V): EncodeJson[K ▶: W] = self.contramap[K ▶: W](_.map { case (k, v) => k -> f(v) })
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
    def filterKeys(p: Predicate[String]):    JsonObject = mapMap(_.filterKeys(p))
    def filterKeysNot(p: Predicate[String]): JsonObject = mapMap(_.filterNot(kv ⇒ p(kv._1)))
    def filterValues(p: Predicate[Json]):    JsonObject = mapMap(_.filter(kv ⇒ p(kv._2)))
    def filterValuesNot(p: Predicate[Json]): JsonObject = mapMap(_.filterNot(kv ⇒ p(kv._2)))

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


    def filterR(p: Predicate[Json]): JsonObject = mapMap(_.collect {
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
    def filterR(p: Predicate[Json]): List[Json] = self.collect { case j if p(j) ⇒ j.filterR(p) }
  }
}

object Descendant {
  type Predicate[A] = A => Boolean

  import argonaut.{JsonFrills, JsonObjectFrills}

//  implicit def descendantAsApplyTraversal[From, Via, To](descendant: Descendant[From, Via, To]):
//    ApplyTraversal[From, From, To, To] = ApplyTraversal(descendant.from, descendant.traversal)

  implicit class DescendantToJsonFrills[From](self: Descendant[From, Json, Json]) {
    def renameFields(fromTos: (String, String)*): From = self.modify(_.renameFields(fromTos: _*))

    def addIfMissing(assocs: Json.JsonAssoc*):   From = self.modify(_.addIfMissing(assocs: _*))

    def removeFields(names: String*): From = self.modify(_.removeFields(names: _*))

    def filterKeys(predicate: Predicate[String]): From = self.modify(_.filterKeys(predicate))
    def filterKeysNot(predicate: Predicate[String]): From = self.modify(_.filterKeysNot(predicate))
    def filterValues(predicate: Predicate[Json]): From = self.modify(_.filterValues(predicate))
    def filterValuesNot(predicate: Predicate[Json]): From = self.modify(_.filterValuesNot(predicate))

    def mapValuesWithKey(f: String => Json => Json): From = self.modify(_.mapValuesWithKey(f))

    def each: Descendant[From, Json, Json] = self composeTraversal objectValuesOrArrayElements
  }

  implicit class DescendantToJsonObjectFrills[From](self: Descendant[From, Json, JsonObject]) {
    def renameFields(fromTos: (String, String)*): From = self.modify(_.renameFields(fromTos: _*))

    def addIfMissing(assocs: Json.JsonAssoc*):   From = self.modify(_.addIfMissing(assocs: _*))

    def removeFields(names: String*): From = self.modify(_.removeFields(names: _*))

    def filterKeys(predicate: Predicate[String]): From = self.modify(_.filterKeys(predicate))
    def filterKeysNot(predicate: Predicate[String]): From = self.modify(_.filterKeysNot(predicate))
    def filterValues(predicate: Predicate[Json]): From = self.modify(_.filterValues(predicate))
    def filterValuesNot(predicate: Predicate[Json]): From = self.modify(_.filterValuesNot(predicate))

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

  def filterObjectP(p: Predicate[Json]): Prism[Json, Json] =
    Prism[Json, Json](json ⇒ Some(json).filter(p))(json ⇒ json)

  val objectValuesOrArrayElements: Traversal[Json, Json] = new PTraversal[Json, Json, Json, Json] {
    def modifyF[F[_]](f: Json ⇒ F[Json])(j: Json)(implicit F: Applicative[F]): F[Json] = j.fold(
      jsonNull   = F.pure(j), jsonBool = _ ⇒ F.pure(j), jsonNumber = _ ⇒ F.pure(j), jsonString = _ ⇒ F.pure(j),
      jsonArray  = arr ⇒ F.map(Each.each[List[Json], Json].modifyF(f)(arr))(Json.array(_: _*)),
      jsonObject = obj ⇒ F.map(Each.each[JsonObject, Json].modifyF(f)(obj))(Json.jObject)
    )
  }

  case class As[From, Via, To, A: CodecJson](from: Descendant[From, Via, To])

  object As {
    implicit def asToDescendant[From, Via, To, A, That](as: As[From, Via, To, A])
      (implicit cpf: CanPrismFrom[To, A, That]): Descendant[From, Via, That] = as.from.composePrism(cpf.prism)
  }
}

case class Descendant[From, Via, To](
  from: From, traversals: List[Traversal[From, To]], ancestorsFn: () => List[(String, Traversal[From, Via])]
) extends Dynamic {
  def bool[That](  implicit cpf: CanPrismFrom[To, Boolean,    That]): Descendant[From, Via, That] = apply(cpf)
  def number[That](implicit cpf: CanPrismFrom[To, JsonNumber, That]): Descendant[From, Via, That] = apply(cpf)
  def string[That](implicit cpf: CanPrismFrom[To, String,     That]): Descendant[From, Via, That] = apply(cpf)
  def array[That]( implicit cpf: CanPrismFrom[To, List[Json], That]): Descendant[From, Via, That] = apply(cpf)
  def obj[That](   implicit cpf: CanPrismFrom[To, JsonObject, That]): Descendant[From, Via, That] = apply(cpf)

  def double[That](    implicit cpf: CanPrismFrom[To, Double,     That]): Descendant[From, Via, That] = apply(cpf)
  def int[That](       implicit cpf: CanPrismFrom[To, Int,        That]): Descendant[From, Via, That] = apply(cpf)
  def float[That](     implicit cpf: CanPrismFrom[To, Float,      That]): Descendant[From, Via, That] = apply(cpf)
  def short[That](     implicit cpf: CanPrismFrom[To, Short,      That]): Descendant[From, Via, That] = apply(cpf)
  def byte[That](      implicit cpf: CanPrismFrom[To, Byte,       That]): Descendant[From, Via, That] = apply(cpf)
  def bigDecimal[That](implicit cpf: CanPrismFrom[To, BigDecimal, That]): Descendant[From, Via, That] = apply(cpf)
  def bigInt[That](    implicit cpf: CanPrismFrom[To, BigInt,     That]): Descendant[From, Via, That] = apply(cpf)

  def as[A: CodecJson]: Descendant.As[From, Via, To, A] = Descendant.As[From, Via, To, A](this)

  def selectDynamic(key: String)(implicit cpf: CanPrismFrom[To, JsonObject, JsonObject]): Descendant[From, Via, Json] =
    obj[JsonObject] composeTraversal FilterIndex.filterIndex(Set(key))

  private def apply[Elem, That](cpf: CanPrismFrom[To, Elem, That]): Descendant[From, Via, That] = composePrism(cpf.prism)

  def composePrism[That](next: Prism[To, That]):         Descendant[From, Via, That] = withTraversal(_ composePrism next)
  def composeTraversal[That](next: Traversal[To, That]): Descendant[From, Via, That] = withTraversal(_ composeTraversal next)
  def composeOptional[That](next: Optional[To, That]):   Descendant[From, Via, That] = withTraversal(_ composeOptional next)
  def composeIso[That](next: Iso[To, That]):             Descendant[From, Via, That] = withTraversal(_ composeIso next)

  def headOption: Option[To] = traversals.flatMap(_.headOption(from)).headOption
  def headOrElse(alternative: => To): To = headOption.getOrElse(alternative)

  def getAll: List[To] = traversals.flatMap(_.getAll(from))

  def set(to: To):         From = foldLeft(_.set(to))
  def modify(f: To => To): From = foldLeft(_.modify(f))

  private def foldLeft(f: Traversal[From, To] => From => From): From = traversals.foldLeft(from) {
    case (acc, traversal) => f(traversal)(acc)
  }

  private def withTraversal[That](fn: Traversal[From, To] => Traversal[From, That]): Descendant[From, Via, That] =
    copy(traversals = traversals.map(fn))
}

case class CanPrismFrom[From, Elem, To](prism: Prism[From, To]) {
  def toList: CanPrismFrom[List[From], Elem, List[To]] =
    CanPrismFrom(Prism[List[From], List[To]](la ⇒ Some(la.flatMap(prism.getOption)))(_.map(prism.reverseGet)))

  // def updateValues[W](f: V ⇒ Option[W]): K ▶: W = self.flatMap(kv ⇒ f(kv._2).map(kv._1 → _))
  def toMap[K]: CanPrismFrom[K ▶: From, Elem, K ▶: To] = CanPrismFrom(Prism[K ▶: From, K ▶: To](mapKA ⇒ {
    Some(for {
      (k, v) <- mapKA
      to    <- prism.getOption(v)
    } yield k -> to)
  })((mapKB: K ▶: To) ⇒ {
    mapKB.map {
      case (k, v) => k -> prism.reverseGet(v)
    }
  }))
}

object CanPrismFrom {
  implicit val cpfJsonToBoolean:    CanPrismFrom[Json, Boolean,    Boolean]    = apply(JsonMonocle.jBoolPrism)
  implicit val cpfJsonToJsonNumber: CanPrismFrom[Json, JsonNumber, JsonNumber] = apply(JsonMonocle.jNumberPrism)
  implicit val cpfJsonToString:     CanPrismFrom[Json, String,     String]     = apply(JsonMonocle.jStringPrism)
  implicit val cpfJsonToJsonArray:  CanPrismFrom[Json, List[Json], List[Json]] = apply(JsonMonocle.jArrayPrism)
  implicit val cpfJsonToJsonObject: CanPrismFrom[Json, JsonObject, JsonObject] = apply(JsonMonocle.jObjectPrism)
  implicit val cpfJsonToBigDecimal: CanPrismFrom[Json, BigDecimal, BigDecimal] = apply(JsonMonocle.jBigDecimalPrism)
//  implicit val cpfJsonToDouble:     CanPrismFrom[Json, Double,     Double]     = apply(jDoublePrism)
//  implicit val cpfJsonToFloat:      CanPrismFrom[Json, Float,      Float]      = apply(jFloatPrism)
  implicit val cpfJsonToBigInt:     CanPrismFrom[Json, BigInt,     BigInt]     = apply(JsonMonocle.jBigIntPrism)
  implicit val cpfJsonToLong:       CanPrismFrom[Json, Long,       Long]       = apply(JsonMonocle.jLongPrism)
  implicit val cpfJsonToInt:        CanPrismFrom[Json, Int,        Int]        = apply(JsonMonocle.jIntPrism)
  implicit val cpfJsonToShort:      CanPrismFrom[Json, Short,      Short]      = apply(JsonMonocle.jShortPrism)
  implicit val cpfJsonToByte:       CanPrismFrom[Json, Byte,       Byte]       = apply(JsonMonocle.jBytePrism)

  implicit def cpfJsonToCodec[A: CodecJson]: CanPrismFrom[Json, A, A] = {
    val A = CodecJson.derived[A]

    apply(Prism[Json, A](json => A.decodeJson(json).toOption)(A.encode))
  }

  implicit def cpfl[From, Elem, To](implicit cpf: CanPrismFrom[From, Elem, To])
    : CanPrismFrom[List[From], Elem, List[To]] = cpf.toList

  implicit def cpfm[From, Elem, To](implicit cpf: CanPrismFrom[From, Elem, To])
    : CanPrismFrom[String ▶: From, Elem, String ▶: To] = cpf.toMap

  implicit def cpfJsonObjectToTypedMap[V](implicit cpf: CanPrismFrom[Json, V, V])
    : CanPrismFrom[JsonObject, V, String ▶: V] = apply(jsonObjectMapIso.composePrism(cpf.toMap[String].prism))

  private val jsonObjectMapIso: Iso[JsonObject, String ▶: Json] =
    Iso[JsonObject, String ▶: Json](_.toMap)(map ⇒ JsonObject.fromTraversableOnce(map))
}