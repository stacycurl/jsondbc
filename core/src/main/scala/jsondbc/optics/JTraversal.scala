package jsondbc
package optics


trait JTraversal[S, A] {
  def getAll(s: S): List[A]
  def headOption(s: S): Option[A]
  def set(a: A): S ⇒ S
  def modify(f: A ⇒ A): S ⇒ S

  def composeOptional[C](optional: JOptional[A, C]): JTraversal[S, C]
  def composeIso[C](iso: JIso[A, C]): JTraversal[S, C]
  def composePrism[C](prism: JPrism[A, C]): JTraversal[S, C]
  def composeTraversal[C](traversal: JTraversal[A, C]): JTraversal[S, C]
}