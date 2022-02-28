package jsondbc
package optics
package monocle
package cats

import _root_.monocle.{Iso, Optional, Prism, Traversal}


trait CatsMonocleOptics extends Optics {
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
