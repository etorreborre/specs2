package org.specs2
package data
import scalaz.{ Tree, TreeLoc }

/**
 * Utility methods for scalaz Trees
 */
private[specs2]
trait Trees { outer => 
  implicit def bottomUpTree[A](t: Tree[A]) = new BottomUpTree(t)
  case class BottomUpTree[A](t: Tree[A]) {
    def bottomUp[B](f: ((A, Stream[B]) => B)) = outer.bottomUp(t, f)
  }
  def bottomUp[A, B](t: Tree[A], f: ((A, Stream[B]) => B)): Tree[B] = {
    val tbs = t.subForest.map(t => bottomUp(t, f))
    new scalaz.Trees{}.node(f(t.rootLabel, tbs.map(_.rootLabel)), tbs)
  }
  
  implicit def extendTreeLoc[T](t: TreeLoc[T]) = new TreeLocx(t)
  case class TreeLocx[T](t: TreeLoc[T]) {
    def parentLocs = outer.parentLocs(t)
  }
  
  def parentLocs[T](t: TreeLoc[T], ps: List[TreeLoc[T]] = Nil): List[TreeLoc[T]] = t.parent match {
    case Some(p) => parentLocs(p, p :: ps)
    case None    => ps
  }
}
private[specs2]
object Trees extends Trees
