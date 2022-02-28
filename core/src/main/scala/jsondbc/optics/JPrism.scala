package jsondbc
package optics

import scala.collection.immutable.List


trait JPrism[S, A] {
  def getOption(s: S): Option[A]
  def reverseGet(a: A): S

  def apply(a: A): S
  def unapply(s: S): Option[A] = getOption(s)

  def modify(f: A ⇒ A): S ⇒ S

  def composeIso[C](iso: JIso[A, C]): JPrism[S, C]

  def composePrism[C](prism: JPrism[A, C]): JPrism[S, C]

  def composeTraversal[C](traversal: JTraversal[A, C]): JTraversal[S, C]

  def toList: JPrism[List[S], List[A]]

  def toMap[K]: JPrism[Map[K, S], Map[K, A]]
}