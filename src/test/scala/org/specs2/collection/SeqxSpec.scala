package org.specs2
package collection
import Seqx._

class SeqxSpec extends Specification { def is =

  "updateLast" ^
  { Seq(1).updateLast(i => i+1) === Seq(2) }         ^
  { Seq(1, 2).updateLast(i => i+1) === Seq(1, 3) }   ^
  { Seq[Int]().updateLast(i => i+1) === Seq[Int]() } ^
                                                     end

}