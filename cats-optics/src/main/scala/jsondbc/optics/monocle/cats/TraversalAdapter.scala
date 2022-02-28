package jsondbc
package optics
package monocle
package cats

import _root_.monocle.{Traversal ⇒ MTraversal}


case class TraversalAdapter[S, A](adapted: MTraversal[S, A]) extends JTraversal[S, A] {
  def composeOptional[C](rhs: JOptional[A, C]): JTraversal[S, C] = rhs match {
    case OptionalAdapter(value) ⇒ TraversalAdapter(adapted composeOptional value)
  }

  def composeIso[C](rhs: JIso[A, C]): JTraversal[S, C] = rhs match {
    case IsoAdapter(value) ⇒ TraversalAdapter(adapted composeIso value)
  }

  def composePrism[C](rhs: JPrism[A, C]): JTraversal[S, C] = rhs match {
    case PrismAdapter(value) ⇒ TraversalAdapter(adapted composePrism value)
  }

  def composeTraversal[C](rhs: JTraversal[A, C]): JTraversal[S, C] = rhs match {
    case TraversalAdapter(value) ⇒ TraversalAdapter(adapted composeTraversal value)
  }

  def getAll(s: S): List[A] = adapted.getAll(s)

  def headOption(s: S): Option[A] = adapted.headOption(s)

  def set(a: A): S ⇒ S = adapted.set(a)

  def modify(f: A ⇒ A): S ⇒ S = adapted.modify(f)
}