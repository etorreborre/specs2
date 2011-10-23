package org.specs2
package reporter
import org.specs2.internal.scalaz.Scalaz
import Scalaz._
import main.Arguments
import time._
import execute._
import specification._
import Statistics._

class StatisticsSpec extends Specification { def is =
                                                                                                                        """
Statistics can be accumulated on each executed specification in order to be displayed at the end of a run
                                                                                                                        """^p^
  "In a specification, the statistics must count"                                                                       ^
    "the number of examples"                                                                                            ! e1^
    "the number of expectations"                                                                                        ! e2^
    "the number of successes"                                                                                           ! e3^
    "the number of failures"                                                                                            ! e4^
    "the number of errors"                                                                                              ! e5^
    "the number of pending"                                                                                             ! e6^
    "the number of skipped"                                                                                             ! e7^
                                                                                                                        p^
  "If there are included specifications"                                                                                ^
    "the total of an inner spec must be ok"                                                                             ! e8^
    "the total of the outer spec must be ok"                                                                            ! e9^
                                                                                                                        end
                                                                                          
  val spec1 = new Specification { def is =
    "spec1.title"           ^
    "e1" ! Success("ok", 2) ^
    "e2" ! failure          ^
    "e3" ! anError          ^
    "e4" ! pending          ^
    "e5" ! skipped          ^ end
  }

  def statistics(s: SpecificationStructure) = foldAll(s.content.fragments.map(f => execute(f)))
  def last(ss: Seq[Stats]) = ss.lastOption.getOrElse(Stats())
  def total(s: SpecificationStructure) = last(statistics(s).totals)
  def totals(s: SpecificationStructure) = statistics(s).totals
  def execute(f: Fragment) = new FragmentExecution {}.executeFragment(Arguments())(f)
    
  def e1 = total(spec1).examples must_== 5
  def e2 = total(spec1).expectations must_== 6
  def e3 = total(spec1).successes must_== 1
  def e4 = total(spec1).failures must_== 1
  def e5 = total(spec1).errors must_== 1
  def e6 = total(spec1).pending must_== 1
  def e7 = total(spec1).skipped must_== 1
  
  val spec2 = new Specification { def is =
    "spec2".title                 ^
    "e1" ! Success("ok", 2)       ^
    "e2" ! failure                ^
    include(spec1)                ^
                                  end
  }
  
  def e8 = {
    val endOfSpecStats = totals(spec2).apply(11) // the end stat for the inner specification
    (endOfSpecStats.examples must_== 5) and (endOfSpecStats.successes must_== 1)
  }

  def e9 = (total(spec2).successes must_== 2) and (total(spec2).examples must_== 7)
}
