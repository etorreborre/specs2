package org.specs2
package data

import org.scalacheck._

import org.specs2.fp._
import Trees._
import org.specs2.matcher.ThrownExpectations

class TreesSpec extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

  We can get all the paths from a tree $paths

"""

  def paths = prop { treeAndPaths: TreeAndPaths =>
    val TreeAndPaths(tree, paths) = treeAndPaths
    tree.allPaths must_== paths
  }

  implicit def ArbitraryTree: Arbitrary[TreeAndPaths] = Arbitrary {
    Gen.choose(1, 5).flatMap(n => genTree(0, (1 to n).toList))
  }

  def genTree(root: Int, nodes: List[Int]): Gen[TreeAndPaths] =
    nodes match {
      case Nil =>
        Gen.const(TreeAndPaths(Tree.Leaf(root), List(List(root))))
      case _ =>
        genTreeList(nodes).map { ls =>
          TreeAndPaths(Tree.Node(root, ls.toStream.map(_.tree)), ls.flatMap(l => l.paths.map(root :: _)))
        }
    }

  def genTreeList(nodes: List[Int]): Gen[List[TreeAndPaths]] =
    nodes match {
      case Nil => Gen.const(Nil)
      case n :: rest =>
        for {
          i <- Gen.choose(0, nodes.size - 1)
          t <- genTree(n, rest.take(i))
          r <- genTreeList(rest.drop(i))
        } yield t :: r
    }

  case class TreeAndPaths(tree: Tree[Int], paths: List[List[Int]]) {
    override def toString =
      tree.drawTree + "\n"
      paths.mkString("\n")
  }
}
