package org.specs2.fp

import TreeLoc.*
import Tree.*
import annotation.tailrec

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
final case class TreeLoc[A](tree: Tree[A], lefts: TreeForest[A],
                            rights: TreeForest[A], parents: Parents[A]):

  /** Select the parent of the current node. */
  def parent: Option[TreeLoc[A]] = parents match
    case (pls, v, prs) #:: ps => Some(loc(Node(v, combChildren(lefts, tree, rights)), pls, prs, ps))
    case LazyList() => None

  /** Select the root node of the tree. */
  @tailrec
  def root: TreeLoc[A] =
    parent match
      case Some(z) => z.root
      case None    => this

  /** Select the left sibling of the current node. */
  def left: Option[TreeLoc[A]] = lefts match
    case t #:: ts   => Some(loc(t, ts, tree #:: rights, parents))
    case LazyList() => None

  /** Select the right sibling of the current node. */
  def right: Option[TreeLoc[A]] = rights match
    case t #:: ts   => Some(loc(t, tree #:: lefts, ts, parents))
    case LazyList() => None

  /** Select the leftmost child of the current node. */
  def firstChild: Option[TreeLoc[A]] = tree.subForest match
    case t #:: ts   => Some(loc(t, LazyList.empty, ts, downParents))
    case LazyList() => None

  /** Select the rightmost child of the current node. */
  def lastChild: Option[TreeLoc[A]] = tree.subForest.reverse match
    case t #:: ts => Some(loc(t, ts, LazyList.empty, downParents))
    case LazyList => None

  /** Select the nth child of the current node. */
  def getChild(n: Int): Option[TreeLoc[A]] =
    for
      lr <- splitChildren(LazyList.empty, tree.subForest, n)
      ls = lr._1
    yield loc(ls.head, ls.tail, lr._2, downParents)

  /** Select the first immediate child of the current node that satisfies the given predicate. */
  def findChild(p: Tree[A] => Boolean): Option[TreeLoc[A]] =
    @tailrec
    def split(acc: TreeForest[A], xs: TreeForest[A]): Option[(TreeForest[A], Tree[A], TreeForest[A])] =
      (acc, xs) match
        case (acc, LazyList.cons(x, xs)) => if p(x) then Some((acc, x, xs)) else split(LazyList.cons(x, acc), xs)
        case _                         => None
    for ltr <- split(LazyList.empty, tree.subForest) yield loc(ltr._2, ltr._1, ltr._3, downParents)

  /** Select the first descendant node of the current node that satisfies the given predicate. */
  def find(p: TreeLoc[A] => Boolean): Option[TreeLoc[A]] =
    cojoin.tree.flatten.find(p)

  /** Get the entire tree represented by this zipper. */
  def toTree: Tree[A] = root.tree

  /** Get the size of the whole tree */
  def size: Int = toTree.size

  /** Get the entire forest represented by this zipper. */
  def toForest: TreeForest[A] = combChildren(root.lefts, root.tree, root.rights)

  /** True if the current node is the root node. */
  def isRoot: Boolean = parents.isEmpty

  /** True if the current node has no left siblings. */
  def isFirst: Boolean = lefts.isEmpty

  /** True if the current node has no right siblings. */
  def isLast: Boolean = rights.isEmpty

  /** True if the current node has no children. */
  def isLeaf: Boolean = tree.subForest.isEmpty

  /** True if the current node is not the root node. */
  def isChild: Boolean = !isRoot

  /** True if the current node has children. */
  def hasChildren: Boolean = !isLeaf

  /** Replace the current node with the given one. */
  def setTree(t: Tree[A]): TreeLoc[A] = loc(t, lefts, rights, parents)

  /** Modify the current node with the given function. */
  def modifyTree(f: Tree[A] => Tree[A]): TreeLoc[A] = setTree(f(tree))

  /** Modify the label at the current node with the given function. */
  def modifyLabel(f: A => A): TreeLoc[A] = setLabel(f(getLabel))

  /** Get the label of the current node. */
  def getLabel: A = tree.rootLabel

  /** Set the label of the current node. */
  def setLabel(a: A): TreeLoc[A] = modifyTree((t: Tree[A]) => Node(a, t.subForest))

  /** Insert the given node to the left of the current node and give it focus. */
  def insertLeft(t: Tree[A]): TreeLoc[A] = loc(t, lefts, LazyList.cons(tree, rights), parents)

  /** Insert the given node to the right of the current node and give it focus. */
  def insertRight(t: Tree[A]): TreeLoc[A] = loc(t, LazyList.cons(tree, lefts), rights, parents)

  /** Insert the given node as the first child of the current node and give it focus. */
  def insertDownFirst(t: Tree[A]): TreeLoc[A] = loc(t, LazyList.empty, tree.subForest, downParents)

  /** Insert the given node as the last child of the current node and give it focus. */
  def insertDownLast(t: Tree[A]): TreeLoc[A] = loc(t, tree.subForest.reverse, LazyList.empty, downParents)

  /** Insert the given node as the nth child of the current node and give it focus. */
  def insertDownAt(n: Int, t: Tree[A]): Option[TreeLoc[A]] =
    for lr <- splitChildren(LazyList.empty, tree.subForest, n) yield loc(t, lr._1, lr._2, downParents)

  /** Delete the current node and all its children. */
  def delete: Option[TreeLoc[A]] = rights match
    case LazyList.cons(t, ts) => Some(loc(t, lefts, ts, parents))
    case _                  => lefts match
      case LazyList.cons(t, ts) => Some(loc(t, ts, rights, parents))
      case _                  => for loc1 <- parent yield loc1.modifyTree((t: Tree[A]) => Node(t.rootLabel, LazyList.empty))

  /**
   * The path from the focus to the root.
   */
  def path: LazyList[A] = getLabel #:: parents.map(_._2)

  /** Maps the given function over the elements. */
  def map[B](f: A => B): TreeLoc[B] =
    val ff = (_: Tree[A]).map(f)
    TreeLoc.loc(tree `map` f, lefts map ff, rights map ff,
      parents.map {
        case (l, t, r) => (l map ff, f(t), r map ff)
      })

  def cojoin: TreeLoc[TreeLoc[A]] =

    val lft = (_: TreeLoc[A]).left
    val rgt = (_: TreeLoc[A]).right
    def dwn[X](tz: TreeLoc[X]): (TreeLoc[X], () => LazyList[TreeLoc[X]]) =
      val f = () => unfold(tz.firstChild) {
        (o: Option[TreeLoc[X]]) => for c <- o yield (c, c.right)
      }
      (tz, f)
    def uf[X](a: TreeLoc[X], f: TreeLoc[X] => Option[TreeLoc[X]]): LazyList[Tree[TreeLoc[X]]] =
      unfold(f(a)) {
        (o: Option[TreeLoc[X]]) => for c <- o yield (Tree.unfoldTree(c)(dwn[X](_: TreeLoc[X])), f(c))
      }

    val p = unfold(parent) {
      (o: Option[TreeLoc[A]]) => for z <- o yield ((uf(z, lft), z, uf(z, rgt)), z.parent)
    }
    TreeLoc.loc(Tree.unfoldTree(this)(dwn(_: TreeLoc[A])), uf(this, lft), uf(this, rgt), p)

  private def downParents = (lefts, tree.rootLabel, rights) #:: parents

  private def combChildren[X](ls: LazyList[X], t: X, rs: LazyList[X]) =
    ls.foldLeft(t #:: rs)((a, b) => b #:: a)

  @tailrec
  private def splitChildren[X](acc: LazyList[X], xs: LazyList[X], n: Int): Option[(LazyList[X], LazyList[X])] =
    (acc, xs, n) match
      case (acc1, xs1, 0)                  => Some((acc1, xs1))
      case (acc1, LazyList.cons(x, xs1), n1) => splitChildren(LazyList.cons(x, acc1), xs1, n1 - 1)
      case _                               => None

object TreeLoc:

  type TreeForest[A] =
    LazyList[Tree[A]]

  type Parent[A] =
    (TreeForest[A], A, TreeForest[A])

  type Parents[A] =
    LazyList[Parent[A]]

  def loc[A](t: Tree[A], l: TreeForest[A], r: TreeForest[A], p: Parents[A]): TreeLoc[A] =
    TreeLoc(t, l, r, p)

  def unfold[A1, B](seed: A1)(f: A1 => Option[(B, A1)]): LazyList[B] =
    f(seed) match
      case None         => LazyList.empty
      case Some((b, a)) => LazyList.cons(b, unfold(a)(f))
