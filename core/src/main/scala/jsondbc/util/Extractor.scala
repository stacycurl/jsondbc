package jsondbc.util

import scala.language.implicitConversions


trait Extractor[A, B] {
  def unapply(a: A): Option[B]
}

object Extractor {
  implicit def apply[A, B](f: A => Option[B]): Extractor[A, B] = (a: A) => f(a)

  def pf[A, B](pf: PartialFunction[A, B]): Extractor[A, B] = (a: A) => pf.lift(a)
}