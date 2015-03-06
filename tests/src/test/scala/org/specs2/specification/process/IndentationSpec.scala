package org.specs2
package specification
package process

import core._
import data._
import org.scalacheck.util.{FreqMap, Pretty}
import org.scalacheck.{Shrink, Arbitrary}
import org.specs2.execute.AsResult
import org.specs2.scalacheck.Parameters
import scalaz._, Scalaz._
import control._
import matcher._
import Arbitraries._
import Functions._
import Fragment._

class IndentationSpec extends Specification with ScalaCheck with TaskMatchers { def is = s2"""

 The Indentation fold is responsible for computing the indentation level in a specification
 based on the presence of tab fragments

 At all times the indentation number
  must never be negative                            $positive
  must be less than or equal to the number of tabs  $lessThanOrEqualTabs
  must be exactly the sum of tabs if no backtabs    $equalTabsWhenNoBacktabs

"""

  def positive = prop { fs: Fragments =>
    indentation(fs) must returnValue((_:Int) must be_>=(0))
  }

  def lessThanOrEqualTabs = prop { fs: Fragments =>
    val tabsNumber = fs.fragments.collect { case Fragment(Tab(n),_,_) => n }.toList.suml
    indentation(fs) must returnValue((_: Int) must be_<=(tabsNumber))
  }

  def equalTabsWhenNoBacktabs = prop { fs: Fragments =>
    val tabsNumber = fs.fragments.collect { case Fragment(Tab(n),_,_) => n }.toList.suml
    indentation(fs.filter(!isBacktab(_))) must returnValue((_:Int) === tabsNumber)
  }

  implicit val prettyFragments = Pretties.prettyFragments

  def indentation(fs: Fragments) =
    Fold.runFoldLast(fs.contents, Fold.fromState(Indentation.fold)(0))
}
