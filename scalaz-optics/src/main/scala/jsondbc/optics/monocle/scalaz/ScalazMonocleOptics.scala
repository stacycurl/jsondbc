package jsondbc
package optics
package monocle
package scalaz

import _root_.monocle.{Iso, Optional, Prism, Traversal}
import _root_.monocle.function.{Each, FilterIndex}


trait ScalazMonocleOptics extends Optics {
  def idTraversal[A]: TraversalAdapter[A, A] =
    adapt(Traversal.id[A])

  def listTraversal[A]: TraversalAdapter[List[A], A] =
    adapt(Each.each(Each.listEach))

  def filterIndexTraversal[A](p: Int ⇒ Boolean): TraversalAdapter[List[A], A] =
    adapt(FilterIndex.filterIndex(p)(FilterIndex.listFilterIndex))

  def optional[S, A](getOption: S ⇒ Option[A])(set: A ⇒ S ⇒ S): OptionalAdapter[S, A] =
    adapt(Optional(getOption)(set))

  def prism[S, A](getOption: S ⇒ Option[A])(reverseGet: A ⇒ S): PrismAdapter[S, A] =
    adapt(Prism(getOption)(reverseGet))

  def iso[S, A](get: S ⇒ A)(reverseGet: A ⇒ S): IsoAdapter[S, A] =
    adapt(Iso(get)(reverseGet))

  protected def adapt[S, A](adapted: Prism[S, A]): PrismAdapter[S, A] =
    PrismAdapter(adapted)

  protected def adapt[S, A](adapted: Iso[S, A]): IsoAdapter[S, A] =
    IsoAdapter(adapted)

  protected def adapt[S, A](adapted: Optional[S, A]): OptionalAdapter[S, A] =
    OptionalAdapter(adapted)

  protected def adapt[S, A](adapted: Traversal[S, A]): TraversalAdapter[S, A] =
    TraversalAdapter(adapted)
}
