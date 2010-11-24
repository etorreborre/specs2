package org.specs2
package reporter

import scalaz._
import Scalaz._
import io._
import main.Arguments
import specification._
import FragmentsShow._

/**
 * A Tree Fold is a FragmentFold which takes a seq of Fragments and transforms it to
 * a Tree of objects of type S.
 * 
 * It uses the Levels fold and an optFold function to create the elements of type S to 
 * include in the final tree. If the optFold function returns none, there will be no 
 * element for the current fragment. This is used in the JUnitDescriptionFold to 
 * avoid any other element than Text, Examples and Steps to be added to the Description
 * tree.
 * 
 * scalaz.TreeLoc is used to build a Tree of nodes S in an immutable way
 *
 */
private[specs2]
object TreeFold {
  def bottomUp[A, B](t: Tree[A], f: ((A, Stream[B]) => B)): Tree[B] = {
    val tbs = t.subForest.map(t => bottomUp(t, f))
    node(f(t.rootLabel, tbs.map(_.rootLabel)), tbs)
  }
}

