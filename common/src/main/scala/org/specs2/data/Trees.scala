package org.specs2
package data

import scalaz.{Tree, TreeLoc}
import scalaz.syntax.foldable._
import scalaz.std.stream._
import Tree._

/**
 * Utility methods for scalaz Trees
 */
trait Trees { outer =>

  /**
   * Implicit definition to add more functionalities to the Tree trait
   */
  implicit class Treex[A](t: Tree[A]) {
    def bottomUp[B](f: ((A, Stream[B]) => B)) = outer.bottomUp(t, f)
    def prune[B](f: A => Option[B]): Option[Tree[B]] = outer.prune(t, f)
    def prune(f: Tree[A] => Option[A])(implicit initial: A): Tree[A] = outer.prune(t, f)(initial)
    def flattenSubForests = outer.flattenSubForests(t)
    def flattenLeft       = outer.flattenLeft(t)
    def size              = t.flatten.size
  }

  /**
   * This implicit can be used to remove None nodes in a Tree
   */
  implicit class CleanedTree[A](t: Tree[Option[A]]) {
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
    Node(f(t.rootLabel, tbs.map(_.rootLabel)), tbs)
  }
  /**
   * remove None nodes from a tree
   */
  def clean[A](t: Tree[Option[A]])(implicit initial: A): Tree[A] = prune(t, (a: Option[A]) => a).getOrElse(Leaf(initial))
  /**
   * remove nodes from a tree if they are None according to a function f
   */
  def prune[A, B](t: Tree[A], f: A => Option[B]): Option[Tree[B]] = {
    val tbs = t.subForest.flatMap(t => prune(t, f))
    f(t.rootLabel).map { root =>
      Node(root, tbs)
    }
  }
  /**
   * remove nodes from a tree if they are None according to a function f
   */
  def prune[A](t: Tree[A], f: Tree[A] => Option[A])(implicit initial: A): Tree[A] = t.cobind(f).clean

  def flattenSubForests[A](tree: Tree[A]): Tree[A] = Node(tree.rootLabel, tree.flattenLeft.drop(1).map(Leaf(_)))

  /**
   * flatten the tree using a foldLeft to avoid SOF
   */
  def flattenLeft[A](tree: Tree[A]): Stream[A] = squishLeft(tree, Stream.Empty)

  /** reimplementation of squish from scalaz, using a foldLeft */
  private def squishLeft[A](tree: Tree[A], xs: Stream[A]): Stream[A] =
    Stream.cons(tree.rootLabel, tree.subForest.reverse.foldl(xs)(s => t => squishLeft(t, s)))

  /**
   * Implicit definition to add more functionalities to the TreeLoc class
   */
  implicit class TreeLocx[T](t: TreeLoc[T]) {
    def parentLocs = outer.parentLocs(t)
    def size = outer.size(t)
    def getParent = t.parent.getOrElse(t)
    def updateLabel(f: T => T) = t.setLabel(f(t.getLabel))
    def addChild(c: T) = t.insertDownLast(Leaf(c)).getParent
    def addFirstChild(c: T) = t.insertDownFirst(Leaf(c)).getParent
    def insertDownLast(c: T) = t.insertDownLast(Leaf(c))
  }

  /**
   * @return the number of nodes in a TreeLoc
   */
  def size[A](t: TreeLoc[A]): Int = t.root.toTree.size

  /**
   * @return the list of all parent locs from a given TreeLoc
   */
  def parentLocs[T](t: TreeLoc[T], ps: Seq[TreeLoc[T]] = Vector()): Seq[TreeLoc[T]] = t.parent match {
    case Some(p) => parentLocs(p, p +: ps)
    case None    => ps
  }

  implicit def treeLocIsSized[T]: Sized[TreeLoc[T]] = new Sized[TreeLoc[T]] {
    def size(t: TreeLoc[T]) : Int = t.size
  }

  implicit def treeIsSized[T]: Sized[Tree[T]] = new Sized[Tree[T]] {
    def size(t: Tree[T]) : Int = t.size
  }
}

object Trees extends Trees
