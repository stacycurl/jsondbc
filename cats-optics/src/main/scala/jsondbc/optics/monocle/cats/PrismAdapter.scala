package jsondbc
package optics
package monocle
package cats

import _root_.monocle.{Prism ⇒ MPrism}


case class PrismAdapter[S, A](adapted: MPrism[S, A]) extends JPrism[S, A] {
  def composeIso[C](rhs: JIso[A, C]): JPrism[S, C] = rhs match {
    case IsoAdapter(value) ⇒ PrismAdapter(adapted composeIso value)
  }

  def composePrism[C](rhs: JPrism[A, C]): JPrism[S, C] = rhs match {
    case PrismAdapter(value) ⇒ PrismAdapter(adapted composePrism value)
  }

  def composeTraversal[C](rhs: JTraversal[A, C]): JTraversal[S, C] = rhs match {
    case TraversalAdapter(value) ⇒ TraversalAdapter(adapted composeTraversal value)
  }

  def apply(a: A): S = adapted.apply(a)

  def getOption(s: S): Option[A] = adapted.getOption(s)

  def reverseGet(a: A): S = adapted.reverseGet(a)

  def modify(f: A ⇒ A): S ⇒ S = adapted.modify(f)

  def toList: JPrism[List[S], List[A]] = PrismAdapter {
    MPrism[List[S], List[A]](ls ⇒ Some(ls.flatMap(adapted.getOption)))(_.map(adapted.reverseGet))
  }

  def toMap[K]: JPrism[Map[K, S], Map[K, A]] = PrismAdapter {
    MPrism[Map[K, S], Map[K, A]](mapKS ⇒ {
      Some(for {
        (k, s) <- mapKS
        a    <- adapted.getOption(s)
      } yield k -> a)
    })((mapKA: Map[K, A]) ⇒ {
      mapKA.map {
        case (k, a) => k -> adapted.reverseGet(a)
      }
    })
  }
}
