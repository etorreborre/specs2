package org.specs2
package reporter
import scalaz.Scalaz
import Scalaz._
import main.Arguments
import execute._
import specification._
import Statistics._

class StatisticsSpec extends SpecificationWithJUnit { def is =
                                                                                          """
  Statistics can be accumulated on each executed specification
  in order to be displayed at the end of a run
                                                                                          """^
  "In a specification, the statistics must count"                                         ^
    "the number of fragments"                                                             ! e1^
    "the number of expectations"                                                          ! e2^
    "the number of successes"                                                             ! e3^
    "the number of failures"                                                              ! e4^
    "the number of errors"                                                                ! e5^
    "the number of pending"                                                               ! e6^
    "the number of skipped"                                                               ! e7^
                                                                                          endp^
  "If there are included specifications"                                                  ^
    "the current statistics must be available"                                            ! e8^
    "the total statistics must be available"                                              ! e9^
                                                                                          end
                                                                                          
  val spec = 
    "start" ^
    "e1" ! Success("ok", 2) ^
    "e2" ! failure          ^
    "e3" ! anError          ^
    "e4" ! pending          ^
    "e5" ! skipped          ^ end

  def statistics(spec: Fragments) = {
    val fragments = Fragments.withSpecStartEnd(spec, "spec").fragments.map(f => execute(f))
    foldAll(fragments)
  }
    
  def total(spec: Fragments) = statistics(spec).total 
  def current(spec: Fragments) = statistics(spec).current
  
  def execute(f: Fragment) = new FragmentExecution {}.executeFragment(Arguments())(f)  
    
  def e1 = total(spec).fragments must_== 5                                                                                           
  def e2 = total(spec).expectations must_== 6                                                                                          
  def e3 = total(spec).successes must_== 1                                                                                          
  def e4 = total(spec).failures must_== 1                                                                                          
  def e5 = total(spec).errors must_== 1                                                                                          
  def e6 = total(spec).pending must_== 1                                                                                          
  def e7 = total(spec).skipped must_== 1
  
  val spec2 = "start"             ^
    "e1" ! Success("ok", 2)       ^
    "e2" ! failure                ^
    fragmentGroup(spec.fragments) ^
                                  end
  
  def e8 = (current(spec).fragments must_== 5) and (current(spec2).fragments must_== 7)
  def e9 = total(spec2).fragments must_== 7
  
}