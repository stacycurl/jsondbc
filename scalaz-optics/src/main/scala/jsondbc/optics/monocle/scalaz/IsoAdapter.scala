package jsondbc
package optics
package monocle
package scalaz

import _root_.monocle.{Iso ⇒ MIso}


case class IsoAdapter[S, A](adapted: MIso[S, A]) extends JIso[S, A] {
  def composeIso[C](rhs: JIso[A, C]): JIso[S, C] = rhs match {
    case IsoAdapter(value) ⇒ IsoAdapter(adapted composeIso value)
  }

  def composePrism[C](rhs: JPrism[A, C]): JPrism[S, C] = rhs match {
    case PrismAdapter(value) ⇒ PrismAdapter(adapted composePrism value)
  }

  def composeOptional[C](rhs: JOptional[A, C]): JOptional[S, C] = rhs match {
    case OptionalAdapter(value) ⇒ OptionalAdapter(adapted composeOptional value)
  }

  def composeTraversal[C](rhs: JTraversal[A, C]): JTraversal[S, C] = rhs match {
    case TraversalAdapter(value) ⇒ TraversalAdapter(adapted composeTraversal value)
  }
}
