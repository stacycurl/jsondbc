package jsondbc

import monocle.function.FilterIndex

import scala.language.{dynamics, higherKinds, implicitConversions}
import monocle.{Iso, Optional, Prism, Traversal}



case class Descendant[From, Via, To](
  from: From, traversals: List[Traversal[From, To]], ancestorsFn: () => List[(String, Traversal[From, Via])]
) extends Dynamic {
  def filterKeys(p: String => Boolean)   (implicit spi: SPI[To]): From = modify(spi.filterKeys(_, p))
  def filterKeysNot(p: String => Boolean)(implicit spi: SPI[To]): From = modify(spi.filterKeysNot(_, p))
  def filterValues(p: To => Boolean)     (implicit spi: SPI[To]): From = modify(spi.filterValues(_, p))
  def filterValuesNot(p: To => Boolean)  (implicit spi: SPI[To]): From = modify(spi.filterValuesNot(_, p))

  def removeFields(names: String*)(implicit spi: SPI[To]): From = modify(spi.removeFields(_, names: _*))
  def renameFields(fromTos: (String, String)*)(implicit spi: SPI[To]): From = modify(spi.renameFields(_, fromTos: _*))
  def addIfMissing(assocs: (String, To)*)(implicit spi: SPI[To]): From = modify(spi.addIfMissing(_, assocs: _*))

  def selectDynamic(key: String)(implicit spi: SPI[To]): Descendant[From, Via, To] =
    composePrism(spi.jObject).composeTraversal(spi.filterObject(Set(key)))

  def array(implicit spi: SPI[To]): Descendant[From, Via, List[To]] = composePrism(spi.jArray)
  def obj(implicit spi: SPI[To]): Descendant[From, Via, spi.JsonObject] = composePrism(spi.jObject)
  def each(implicit spi: SPI[To]): Descendant[From, Via, To] = composeTraversal(spi.jDescendants)

  def bool[That](  implicit cpf: CanPrismFrom[To, Boolean,    That]): Descendant[From, Via, That] = apply(cpf)
  def string[That](implicit cpf: CanPrismFrom[To, String,     That]): Descendant[From, Via, That] = apply(cpf)

  def double[That](    implicit cpf: CanPrismFrom[To, Double,     That]): Descendant[From, Via, That] = apply(cpf)
  def int[That](       implicit cpf: CanPrismFrom[To, Int,        That]): Descendant[From, Via, That] = apply(cpf)
  def float[That](     implicit cpf: CanPrismFrom[To, Float,      That]): Descendant[From, Via, That] = apply(cpf)
  def short[That](     implicit cpf: CanPrismFrom[To, Short,      That]): Descendant[From, Via, That] = apply(cpf)
  def byte[That](      implicit cpf: CanPrismFrom[To, Byte,       That]): Descendant[From, Via, That] = apply(cpf)
  def bigDecimal[That](implicit cpf: CanPrismFrom[To, BigDecimal, That]): Descendant[From, Via, That] = apply(cpf)
  def bigInt[That](    implicit cpf: CanPrismFrom[To, BigInt,     That]): Descendant[From, Via, That] = apply(cpf)

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