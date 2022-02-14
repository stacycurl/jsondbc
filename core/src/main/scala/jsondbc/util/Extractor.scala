package jsondbc
package util

import scala.language.implicitConversions

import monocle.Prism

import scala.util.Try


trait Extractor[A, B] {
  def unapply(a: A): Option[B]

  final def map[C](f: B => C): Extractor[A, C] = Extractor.Mapped(this, f)
}

object Extractor {
  implicit def apply[A, B](f: A => Option[B]): Extractor[A, B] = Fn(f)

  def pf[A, B](pf: PartialFunction[A, B]): Extractor[A, B] = (a: A) => pf.lift(a)
  
  def prism[A, B](prism: Prism[A, B]): Extractor[A, B] = (a: A) ⇒ prism.getOption(a)

  private case class Fn[A, B](f: A => Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = f(a)
  }

  private case class Mapped[A, B, C](from: Extractor[A, B], f: B => C) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = from.unapply(a).map(f)
  }

  val Int: Extractor[String, Int] =
    Extractor[String, Int](value => Try(value.toInt).toOption)
}