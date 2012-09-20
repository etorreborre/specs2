package org.specs2
package data

import org.specs2.internal.scalaz._
import Trees._
import Scalaz._
import Tree._
import text.Trim._
import matcher.DataTables

class TreesSpec extends Specification with DataTables { def is =

  "A tree can be pruned by providing a function mapping nodes to Option[Node]"                                          ^
    "if a node is mapped to Some(n), it stays in the tree"                                                              ! e1^
    "if a node is mapped to None, it is removed from the tree"                                                          ^
      "if it's a leaf"                                                                                                  ! e2^
      "if it's a subtree"                                                                                               ! e3^
      "even the root of the tree"                                                                                       ! e4^
                                                                                                                        endp^
  "A TreeLoc can"                                                                                                       ^
    "return its size"                                                                                                   ! e5^
    "be added a new child"                                                                                              ! e6^
                                                                                                                        p^
  "A Tree can be flattenLeft to avoid SOF"                                                                              ! e7^
                                                                                                                        end

  /**
   *  tree is:
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
  def tree = node(0, node(2, leaf(1)) :: node(3, leaf(4)) :: leaf(5) :: Nil)

  def tree1 = node(0, node(2, leaf(2)))
  def tree2 = node(0, node(2, leaf(1)))
  def tree3 = node(0, node(1, leaf(2)))
  def tree4 = node(1, node(2, leaf(1)))
  def tree5 = node(0, Stream.cons(leaf(3), node(2, leaf(2))))

  val prune = (i: Int) => if (i % 2 == 0) Some(i) else None

  def e1 = pruneAndDraw(tree1, prune) must beTree(
  "0",
  "|",
  "`- 2",
  "   |",
  "   `- 2")

  def e2 = pruneAndDraw(tree2, prune) must beTree(
  "0",
  "|",
  "`- 2")

  def e3 = pruneAndDraw(tree3, prune) must beTree("0")
  def e4 = pruneAndDraw(tree4, prune) must beTree("None")

  def e5 = {
     "tree"  | "size" |>
      tree   ! 6      |
      tree1  ! 3      |
      tree2  ! 3      |
      tree3  ! 3      | { (tree, size) => tree.loc.size must_== size }
  }

  // note that the TreeLoc stays at the root after the addition of the child node
  def e6 = tree1.loc.addChild(3).tree.drawTree must beTree(
  "0",
  "|",
  "+- 2",
  "|  |",
  "|  `- 2",
  "|",
  "`- 3")

  def e7 = {
    val tree = tree3.loc.addChild(4).tree
    tree.flattenLeft.toSeq aka "flattenLeft" must_== tree.flatten.toSeq
  }

  def pruneAndDraw(tree: Tree[Int], f: Int => Option[Int]) = Option(tree).map(_.drawTree).getOrElse("None\n")
  def beTree(s: String*) = be_==(s.mkString("", "\n", "\n"))

//  def drawTree2[A](tree: Tree[A])(implicit sh: Show[A]): String =
//    Foldable[Stream].foldMap(draw)((_: String) + "\n")

  implicit def anyToStream[A](a: A): Stream[A] = Stream(a)
  implicit def listToStream[A](a: List[A]): Stream[A] = a.toStream
}
