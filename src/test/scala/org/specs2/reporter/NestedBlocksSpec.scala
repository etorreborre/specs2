package org.specs2
package reporter
import NestedBlocks._
import specification._
import Fragments._

class NestedBlocksSpec extends Specification { def is =
                                                                                                          """
  associateStartEnd takes a seq of blocks and associating start and end blocks with a function:
     f: (start, end) => (updatedStart, updatedEnd)
  
  It is used to copy the computed statistics on ExecutedSpecEnd fragments to the corresponding 
  ExecutedSpecStart fragments.
                                                                                                          """ ! e1^
  "It works also when several specs are included in a parent one"                                             ! e2^
  "It works also when several specs are included in a parent one"                                             ! e3^
                                                                                                              end

  val subspec  = new Specification { def is = "sub1".title ^ "e1-1" ! success }
  val subspec2 = new Specification { def is = "sub2".title ^ "e2-1" ! success }
  val subspec3 = new Specification { def is = "sub3".title ^ "e3-1" ! success }

  val spec  = "spec".title ^ "text" ^ subspec ^ "e" ! success
  val spec2 = "spec".title ^ "text" ^ subspec ^ subspec2 ^ "e" ! success
  val spec3 = "spec".title ^ "text" ^ subspec ^ subspec2 ^ subspec3 ^ "e" ! success

  def swap = (start: Fragment, end: Fragment) => (start, end) match {
    case (SpecStart(_,_,_,_,_), SpecEnd(en)) => (SpecStart(SpecName(en.title+"-swapped")), SpecEnd(en))
    case other                               => other
  }
  
  def e1 = associate(spec, List("spec-swapped", "sub1-swapped", "sub1", "spec"))

  def e2 = associate(spec2, List("spec-swapped", "sub1-swapped", "sub1", "sub2-swapped", "sub2", "spec"))

  def e3 = associate(spec3, List("spec-swapped", "sub1-swapped", "sub1", "sub2-swapped", "sub2", "sub3-swapped", "sub3", "spec"))

  def associate(s: Fragments, expected: Seq[String]) = associateStartEnd(s.fragments map fragmentsToSpecBlock, swap) collect {
    case SpecStart(n,_,_,_,_) => n.title; case SpecEnd(n) => n.title } must_== expected
}