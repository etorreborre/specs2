package org.specs2
package reporter
import NestedBlocks._
import specification._

class NestedBlockSpec extends Specification { def is =
                                                                                                          """
  associateStartEnd takes a seq of blocks and associating start and end blocks with a function:
     f: (start, end) => (updatedStart, updatedEnd)
  
  It is used to copy the computed statistics on ExecutedSpecEnd fragments to the corresponding 
  ExecutedSpecStart fragments.
                                                                                                          """ ! e1

  val subspec = new Specification { def is = "sub".title ^ "e1" ! success }
  val spec = "spec".title ^ "text" ^ include(subspec) ^ "e2" ! success
  
  def swap = (start: Fragment, end: Fragment) => (end, start)
  
  def e1 = associateStartEnd(spec.fragments map fragmentsToSpecBlock, swap).map(_.value) must
           beLike { case SpecEnd(_) :: rest if rest.size == 6 => ok }
}