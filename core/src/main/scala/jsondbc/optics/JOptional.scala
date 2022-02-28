package jsondbc
package optics


trait JOptional[S, A] {
  def composeIso[C](iso: JIso[A, C]): JOptional[S, C]

  def composePrism[C](prism: JPrism[A, C]): JOptional[S, C]

  def composeOptional[C](optional: JOptional[A, C]): JOptional[S, C]

  def composeTraversal[C](traversal: JTraversal[A, C]): JTraversal[S, C]


  def modify(f: A ⇒ A): S ⇒ S
}