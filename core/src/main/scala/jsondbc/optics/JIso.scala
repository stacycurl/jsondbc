package jsondbc
package optics


trait JIso[S, A] {
  def composeOptional[C](optional: JOptional[A, C]): JOptional[S, C]
  def composeIso[C](iso: JIso[A, C]): JIso[S, C]
  def composePrism[C](prism: JPrism[A, C]): JPrism[S, C]
  def composeTraversal[C](traversal: JTraversal[A, C]): JTraversal[S, C]
}
