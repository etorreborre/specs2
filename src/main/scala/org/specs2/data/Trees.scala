package org.specs2
package data
import scalaz.{ Tree, TreeLoc }

/**
 * Utility methods for scalaz Trees
 */
private[specs2]
trait Trees { outer =>

  /**
   * Implicit definition to add a bottomUp method on Trees
   */
  implicit def bottomUpTree[A](t: Tree[A]) = new BottomUpTree(t)
  case class BottomUpTree[A](t: Tree[A]) {
    def bottomUp[B](f: ((A, Stream[B]) => B)) = outer.bottomUp(t, f)
  }
  /**
   * map a Tree from leaves to root by replacing each node with the result of a function taking
   * that node and the mapping of all its children.
   *
   * This is used in JUnit to map a Tree[Description] where no Description objects are related to a Tree[Description]
   * where each node returns the children nodes on the "getChildren" method
   */
  def bottomUp[A, B](t: Tree[A], f: ((A, Stream[B]) => B)): Tree[B] = {
    val tbs = t.subForest.map(t => bottomUp(t, f))
    new scalaz.Trees{}.node(f(t.rootLabel, tbs.map(_.rootLabel)), tbs)
  }

  /**
   * Implicit method to add a parentLoc method to TreeLoc[T]
   */
  implicit def extendTreeLoc[T](t: TreeLoc[T]) = new TreeLocx(t)
  case class TreeLocx[T](t: TreeLoc[T]) {
    def parentLocs = outer.parentLocs(t)
  }

  /**
   * @return the list of all parent locs from a given TreeLoc
   */
  def parentLocs[T](t: TreeLoc[T], ps: List[TreeLoc[T]] = Nil): List[TreeLoc[T]] = t.parent match {
    case Some(p) => parentLocs(p, p :: ps)
    case None    => ps
  }
}
private[specs2]
object Trees extends Trees
