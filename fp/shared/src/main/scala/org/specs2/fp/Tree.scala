package org.specs2.fp

import Tree._
import syntax._

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
sealed abstract class Tree[A] {

  /** The label at the root of this tree. */
  def rootLabel: A

  /** The child nodes of this tree. */
  def subForest: Stream[Tree[A]]

  /** Maps the elements of the Tree into a Monoid and folds the resulting Tree. */
  def foldMap[B : Monoid](f: A => B): B =
    f(rootLabel) |+| subForest.map(_.foldMap(f)).sumAll

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    Foldable[Stream].foldRight(flatten, z)(f)

  /** A 2D String representation of this Tree. */
  def drawTree(implicit sh: Show[A]): String = {
    val reversedLines = draw
    val first = new StringBuilder(reversedLines.head.toString.reverse)
    val rest = reversedLines.tail
    rest.foldLeft(first) { (acc, elem) =>
      acc.append("\n").append(elem.toString.reverse)
    }.append("\n").toString
  }

  /** A histomorphic transform. Each element in the resulting tree
   * is a function of the corresponding element in this tree
   * and the histomorphic transform of its children.
   **/
  def scanr[B](g: (A, Stream[Tree[B]]) => B): Tree[B] = {
    val c = Need(subForest.map(_.scanr(g)))
    Node(g(rootLabel, c.value), c.value)
  }

  /** A 2D String representation of this Tree, separated into lines.
   * Uses reversed StringBuilders for performance, because they are
   * prepended to.
   **/
  private def draw(implicit sh: Show[A]): Vector[StringBuilder] = {
    val branch = " -+" // "+- ".reverse
    val stem = " -`" // "`- ".reverse
    val trunk = "  |" // "|  ".reverse

    def drawSubTrees(s: Stream[Tree[A]]): Vector[StringBuilder] = s match {
      case ts if ts.isEmpty       => Vector.empty[StringBuilder]
      case t #:: ts if ts.isEmpty => new StringBuilder("|") +: shift(stem, "   ", t.draw)
      case t #:: ts               =>
        new StringBuilder("|") +: (shift(branch, trunk, t.draw) ++ drawSubTrees(ts))
    }

    def shift(first: String, other: String, s: Vector[StringBuilder]): Vector[StringBuilder] = {
      var i = 0
      while (i < s.length) {
        if (i == 0) s(i).append(first)
        else s(i).append(other)
        i += 1
      }
      s
    }

    new StringBuilder(sh.show(rootLabel).reverse) +: drawSubTrees(subForest)
  }

  /** Pre-order traversal. */
  def flatten: Stream[A] = {
    def squish(tree: Tree[A], xs: Stream[A]): Stream[A] =
      Stream.cons(tree.rootLabel, Foldable[Stream].foldRight(tree.subForest, xs)(squish(_, _)))

    squish(this, Stream.Empty)
  }

  /** Breadth-first traversal. */
  def levels: Stream[Stream[A]] = {
    val f = (s: Stream[Tree[A]]) => {
      Foldable[Stream].foldMap(s)((_: Tree[A]).subForest)
    }
    Stream.iterate(Stream(this))(f) takeWhile (_.nonEmpty) map (_ map (_.rootLabel))
  }

  /** Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: Tree[A] => B): Tree[B] = unfoldTree(this)(t => (f(t), () => t.subForest))

  /** A TreeLoc zipper of this tree, focused on the root node. */
  def loc: TreeLoc[A] = TreeLoc.loc(this, Stream.Empty, Stream.Empty, Stream.Empty)

  /** Turns a tree of pairs into a pair of trees. */
  def unzip[A1, A2](implicit p: A => (A1, A2)): (Tree[A1], Tree[A2]) = {
    val uz = Need(subForest.map(_.unzip))
    val fst = Need(uz.value map (_._1))
    val snd = Need(uz.value map (_._2))
    (Node(rootLabel._1, fst.value), Node(rootLabel._2, snd.value))
  }

  def foldNode[Z](f: A => Stream[Tree[A]] => Z): Z =
    f(rootLabel)(subForest)

  def map[B](f: A => B): Tree[B] =
    Node(f(rootLabel), subForest map (_ map f))

  def flatMap[B](f: A => Tree[B]): Tree[B] = {
    val r: Tree[B] = f(rootLabel)
    Node(r.rootLabel, r.subForest #::: subForest.map(_.flatMap(f)))
  }

}

object Tree {
  def apply[A](root: => A): Tree[A] = Leaf(root)

  object Node {
    def apply[A](root: => A, forest: => Stream[Tree[A]]): Tree[A] = {
      new Tree[A] {
        private[this] val rootc = Need(root)
        private[this] val forestc = Need(forest)
        def rootLabel = rootc.value
        def subForest = forestc.value

        override def toString = "<tree>"
      }
    }

    def unapply[A](t: Tree[A]): Option[(A, Stream[Tree[A]])] = Some((t.rootLabel, t.subForest))
  }

  object Leaf {
    def apply[A](root: => A): Tree[A] = {
      Node(root, Stream.empty)
    }

    def unapply[A](t: Tree[A]): Option[A] = {
      t match {
        case Node(root, Stream.Empty) =>
          Some(root)
        case _ =>
          None
      }
    }
  }

  def unfoldForest[A, B](s: Stream[A])(f: A => (B, () => Stream[A])): Stream[Tree[B]] =
    s.map(unfoldTree(_)(f))

  def unfoldTree[A, B](v: A)(f: A => (B, () => Stream[A])): Tree[B] =
    f(v) match {
      case (a, bs) => Node(a, unfoldForest(bs.apply())(f))
    }
}
