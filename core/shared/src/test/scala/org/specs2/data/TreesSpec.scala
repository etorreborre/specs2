package org.specs2
package data

import fp._
import Tree._
import Trees._
import matcher._

class TreesSpec extends Specification with DataTables with Expectations { def is = s2"""

 # A tree can be pruned by providing a function mapping Nodes to Option[Node]
    if a Node is mapped to Some(n), it stays in the tree             $pruning1
    if a Node is mapped to None, it is removed from the tree
      if it's a Leaf                                                 $pruning2
      if it's a subtree                                              $pruning3
      even the root of the tree                                      $pruning4

 # A TreeLoc can
    return its size                                                  $treeLoc1
    be added a new child                                             $treeLoc2

  A Tree can be flattenLeft to avoid SOF                             $flattenTree1

"""

  val prune = (i: Int) => if i % 2 == 0 then Some(i) else None

  def pruning1 = pruneAndDraw(tree1, prune) must beTree(
    "0",
    "|",
    "`- 2",
    "   |",
    "   `- 2")

  def pruning2 = pruneAndDraw(tree2, prune) must beTree(
    "0",
    "|",
    "`- 2")

  def pruning3 = pruneAndDraw(tree3, prune) must beTree("0")
  def pruning4 = pruneAndDraw(tree4, prune) must beTree("None")

  def treeLoc1 =
   "tree"  | "size" |>
    tree   ! 6      |
    tree1  ! 3      |
    tree2  ! 3      |
    tree3  ! 3      | { (tree, size) => tree.loc.size must ===(size) }

  // note that the TreeLoc stays at the root after the addition of the child Node
  def treeLoc2 = tree1.loc.addChild(3).tree.drawTree must beTree(
    "0",
    "|",
    "+- 2",
    "|  |",
    "|  `- 2",
    "|",
    "`- 3")

  def flattenTree1 =
    val tree = tree3.loc.addChild(4).tree
    tree.flattenLeft aka "flattenLeft" must ===(tree.flatten)


 /**
  *  the tree is:
  *
  *  0
  *  |
  *  +- 2
  *  |  |
  *  |  `- 1
  *  |
  *  +- 3
  *  |  |
  *  |  `- 4
  *  |
  *  `- 5
  */
  def tree = Node(0, Node(2, Leaf(1)) :: Node(3, Leaf(4)) :: Leaf(5) :: Nil)

  def tree1 = Node(0, Node(2, Leaf(2)))
  def tree2 = Node(0, Node(2, Leaf(1)))
  def tree3 = Node(0, Node(1, Leaf(2)))
  def tree4 = Node(1, Node(2, Leaf(1)))
  def tree5 = Node(0, LazyList.cons(Leaf(3), Node(2, Leaf(2))))


  def pruneAndDraw(tree: Tree[Int], f: Int => Option[Int]) = tree.prune(f).map(_.drawTree).getOrElse("None\n")
  def beTree(s: String*) = be_==(s.mkString("", "\n", "\n"))

  implicit def anyToStream[A](a: A): LazyList[A] = LazyList(a)
  implicit def listToStream[A](as: List[A]): LazyList[A] = LazyList(as:_*)
}
