package org.specs2.fp

import Tree._
import syntax._

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
sealed abstract class Tree[A]:

  /** The label at the root of this tree. */
  def rootLabel: A

  /** The child nodes of this tree. */
  def subForest: LazyList[Tree[A]]

  /** Maps the elements of the Tree into a Monoid and folds the resulting Tree. */
  def foldMap[B : Monoid](f: A => B): B =
    f(rootLabel) |+| subForest.map(_.foldMap(f)).sumAll

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    Foldable[LazyList].foldRight(flatten, z)(f)

  /** A 2D String representation of this Tree. */
  def drawTree(implicit sh: Show[A]): String =
    val reversedLines = draw
    val first = new StringBuilder(reversedLines.head.toString.reverse)
    val rest = reversedLines.tail
    rest.foldLeft(first) { (acc, elem) =>
      acc.append("\n").append(elem.toString.reverse)
    }.append("\n").toString

  /** A histomorphic transform. Each element in the resulting tree
   * is a function of the corresponding element in this tree
   * and the histomorphic transform of its children.
   **/
  def scanr[B](g: (A, LazyList[Tree[B]]) => B): Tree[B] =
    val c = Need(subForest.map(_.scanr(g)))
    Node(g(rootLabel, c.value), c.value)

  /** A 2D String representation of this Tree, separated into lines.
   * Uses reversed StringBuilders for performance, because they are
   * prepended to.
   **/
  private def draw(implicit sh: Show[A]): Vector[StringBuilder] =
    val branch = " -+" // "+- ".reverse
    val stem = " -`" // "`- ".reverse
    val trunk = "  |" // "|  ".reverse

    def drawSubTrees(s: LazyList[Tree[A]]): Vector[StringBuilder] = s match
      case ts if ts.isEmpty       => Vector.empty[StringBuilder]
      case t #:: ts if ts.isEmpty => new StringBuilder("|") +: shift(stem, "   ", t.draw)
      case t #:: ts               =>
        new StringBuilder("|") +: (shift(branch, trunk, t.draw) ++ drawSubTrees(ts))

    def shift(first: String, other: String, s: Vector[StringBuilder]): Vector[StringBuilder] =
      var i = 0
      while i < s.length do
        if i == 0 then s(i).append(first)
        else s(i).append(other)
        i += 1
      s

    new StringBuilder(sh.show(rootLabel).reverse) +: drawSubTrees(subForest)

  /** Pre-order traversal. */
  def flatten: LazyList[A] =
    def squish(tree: Tree[A], xs: LazyList[A]): LazyList[A] =
      LazyList.cons(tree.rootLabel, Foldable[LazyList].foldRight(tree.subForest, xs)(squish(_, _)))

    squish(this, LazyList.empty)

  /** Breadth-first traversal. */
  def levels: LazyList[LazyList[A]] =
    val f = (s: LazyList[Tree[A]]) => {
      Foldable[LazyList].foldMap(s)((_: Tree[A]).subForest)
    }
    LazyList.iterate(LazyList(this))(f) takeWhile (_.nonEmpty) map (_ map (_.rootLabel))

  /** Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: Tree[A] => B): Tree[B] = unfoldTree(this)(t => (f(t), () => t.subForest))

  /** A TreeLoc zipper of this tree, focused on the root node. */
  def loc: TreeLoc[A] = TreeLoc.loc(this, LazyList.empty, LazyList.empty, LazyList.empty)

  /** Turns a tree of pairs into a pair of trees. */
  def unzip[A1, A2](implicit p: A => (A1, A2)): (Tree[A1], Tree[A2]) =
    val uz = Need(subForest.map(_.unzip))
    val fst = Need(uz.value map (_._1))
    val snd = Need(uz.value map (_._2))
    (Node(rootLabel._1, fst.value), Node(rootLabel._2, snd.value))

  def foldNode[Z](f: A => LazyList[Tree[A]] => Z): Z =
    f(rootLabel)(subForest)

  def map[B](f: A => B): Tree[B] =
    Node(f(rootLabel), subForest map (_ map f))

  def flatMap[B](f: A => Tree[B]): Tree[B] =
    val r: Tree[B] = f(rootLabel)
    Node(r.rootLabel, r.subForest #::: subForest.map(_.flatMap(f)))


object Tree:
  def apply[A](root: => A): Tree[A] = Leaf(root)

  object Node:
    def apply[A](root: => A, forest: => LazyList[Tree[A]]): Tree[A] =
      new Tree[A] {
        private val rootc = Need(root)
        private val forestc = Need(forest)
        def rootLabel = rootc.value
        def subForest = forestc.value

        override def toString = "<tree>"
      }

    def unapply[A](t: Tree[A]): Option[(A, LazyList[Tree[A]])] = Some((t.rootLabel, t.subForest))

  object Leaf:
    def apply[A](root: => A): Tree[A] =
      Node(root, LazyList.empty)

    def unapply[A](t: Tree[A]): Option[A] =
      t match
        case Node(root, LazyList()) =>
          Some(root)
        case _ =>
          None

  def unfoldForest[A, B](s: LazyList[A])(f: A => (B, () => LazyList[A])): LazyList[Tree[B]] =
    s.map(unfoldTree(_)(f))

  def unfoldTree[A, B](v: A)(f: A => (B, () => LazyList[A])): Tree[B] =
    f(v) match
      case (a, bs) => Node(a, unfoldForest(bs.apply())(f))
