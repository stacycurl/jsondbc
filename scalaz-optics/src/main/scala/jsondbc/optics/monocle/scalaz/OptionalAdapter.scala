package jsondbc
package optics
package monocle
package scalaz

import _root_.monocle.{Optional ⇒ MOptional}


case class OptionalAdapter[S, A](adapted: MOptional[S, A]) extends JOptional[S, A] {
  def composeIso[C](rhs: JIso[A, C]): JOptional[S, C] = rhs match {
    case IsoAdapter(value) ⇒ OptionalAdapter(adapted composeIso value)
  }

  def composePrism[C](rhs: JPrism[A, C]): JOptional[S, C] = rhs match {
    case PrismAdapter(value) ⇒ OptionalAdapter(adapted composePrism value)
  }

  def composeOptional[C](rhs: JOptional[A, C]): JOptional[S, C] = rhs match {
    case OptionalAdapter(value) ⇒ OptionalAdapter(adapted composeOptional  value)
  }

  def composeTraversal[C](rhs: JTraversal[A, C]): JTraversal[S, C] = rhs match {
    case TraversalAdapter(value) ⇒ TraversalAdapter(adapted composeTraversal value)
  }

  def modify(f: A ⇒ A): S ⇒ S = adapted.modify(f)
}
