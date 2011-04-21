package org.specs2
package data
import scalaz.{NonEmptyList, Tree, TreeLoc, Scalaz}
import Scalaz._

/**
 * Utility methods for scalaz Trees
 */
private[specs2]
trait Trees { outer =>

  /**
   * Implicit definition to add a bottomUp method on Trees
   */
  implicit def extendedTree[A](t: Tree[A]) = Treex(t)
  case class Treex[A](t: Tree[A]) {
    def bottomUp[B](f: ((A, Stream[B]) => B)) = outer.bottomUp(t, f)
    def prune[B](f: A => Option[B]): Option[Tree[B]] = outer.prune(t, f)
    def prune(f: Tree[A] => Option[A])(implicit initial: A): Tree[A] = outer.prune(t, f)(initial)
    def flattenSubForests = outer.flattenSubForests(t)

  }

  /**
   * This implicit can be used to remove None nodes in a Tree
   */
  implicit def cleanedTree[A](t: Tree[Option[A]]) = CleanedTree(t)
  case class CleanedTree[A](t: Tree[Option[A]]) {
    def clean(implicit initial: A): Tree[A] = outer.clean(t)(initial)
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
    node(f(t.rootLabel, tbs.map(_.rootLabel)), tbs)
  }
  /**
   * remove None nodes from a tree
   */
  def clean[A](t: Tree[Option[A]])(implicit initial: A): Tree[A] = prune(t, (a: Option[A]) => a).getOrElse(leaf(initial))
  /**
   * remove nodes from a tree if they are None according to a function f
   */
  def prune[A, B](t: Tree[A], f: A => Option[B]): Option[Tree[B]] = {
    val tbs = t.subForest.flatMap(t => prune(t, f))
    f(t.rootLabel).map { root =>
      node(root, tbs)
    }
  }
  /**
   * remove nodes from a tree if they are None according to a function f
   */
  def prune[A](t: Tree[A], f: Tree[A] => Option[A])(implicit initial: A): Tree[A] = t.cobind(f).clean

  def flattenSubForests[A](tree: Tree[A]): Tree[A] = node(tree.rootLabel, tree.flatten.drop(1).map(leaf(_)))

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
