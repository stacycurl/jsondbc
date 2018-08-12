package jsondbc.util

trait Extractor[A, B] {
  def unapply(a: A): Option[B]
}

object Extractor {
  def pf[A, B](pf: PartialFunction[A, B]): Extractor[A, B] = (a: A) => pf.lift(a)
}