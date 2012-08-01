package org.specs2
package specification

/**
 * This trait models "what happens" before a fragment is created in terms of side-effects.
 *
 * For now it is mostly used for unit specifications where side-effects can occur inside "blocks" when creating
 * examples. We suppose that acceptance specifications do not have side-effects at creation time.
 */
trait SpecificationNavigation {

  /**
   * @return the list of all fragments
   */
  private[specs2]
  def content: Fragments

  /**
   * @return the list of fragments which have been created before a given one
   */
  private[specs2]
  def fragmentsTo(f: Fragment): Seq[Fragment] = {
    val path = f match {
      case e @ Example(_,_) => e.creationPath
      case a @ Action(_)    => a.creationPath
      case other            => None
    }
    val fragments = content.fragments
    fragments.take(path.flatMap(_.path.lastOption).getOrElse(0) + 1)
  }
}

sealed trait CreationPath {
  def path: Seq[Int]
  def startsWith(other: CreationPath) = path.take(other.path.size) == other.path

  override def equals(a: Any) = {
    a match {
      case creationPath: CreationPath => creationPath.path == path
      case other                      => false
    }
  }
}
case class MutableCreationPath(path: Seq[Int]) extends CreationPath
case class AcceptanceCreationPath(path: Seq[Int]) extends CreationPath