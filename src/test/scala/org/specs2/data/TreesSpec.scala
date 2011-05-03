package org.specs2
package data

import scalaz._
import Scalaz._
import Trees._
import text.Trim._

class TreesSpec extends Specification { def is =

  "A tree can be pruned by providing a function mapping nodes to Option[Node]"                         ^
    "if a node is mapped to Some(n), it stays in the tree"                                             ! e1^
    "if a node is mapped to None, it is removed from the tree"                                         ^
      "if it's a leaf"                                                                                 ! e2^
      "if it's a subtree"                                                                              ! e3^
      "even the root of the tree"                                                                      ! e4^
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

  def pruneAndDraw(tree: Tree[Int], f: Int => Option[Int]) = tree.prune(f).map(_.drawTree).getOrElse("None\n")
  def beTree(s: String*) = be_==(s.mkString("", "\n", "\n"))
  
  implicit def anyToStream[A](a: A): Stream[A] = Stream(a)
  implicit def listToStream[A](a: List[A]): Stream[A] = a.toStream
}