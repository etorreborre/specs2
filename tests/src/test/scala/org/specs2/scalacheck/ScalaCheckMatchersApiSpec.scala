package org.specs2
package scalacheck

import org.scalacheck.Prop._
import org.scalacheck.util.{FreqMap, Pretty}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.specs2.Specification

class ScalaCheckMatchersApiSpec extends Specification with ScalaCheck { def is = s2"""
 There are various ways to use a property in specs2

 A ScalaCheck property can be used in the body of an Example
 ${ forAll { (i:Int) => i > 0 || i <= 0 } }

 The `prop` method can be used to create a property from a function
    returning a result
    ${ prop { (i: Int) => success} }
    returning a match result
    ${ prop { (i: Int) => i must be_>(0) or be_<=(0) } }
    returning a boolean value
    ${ prop { (i: Int) => i > 0 || i <= 0 } }
    using  an implication and a match result
    ${ prop { i: Int => (i > 0) ==> (i must be_>(0)) } } $xtag
    ${ prop { (i: Int, j: Int) => (i > j) ==> (i must be_>(j)) } }
    using an implication and a boolean value
    ${ prop { (i:Int) => (i > 0) ==> (i > 0) } }

 It is possible to specify typeclass instances for Arbitrary, Shrink, Pretty and collect
   Arbitrary
   to specify a specific arbitrary instance for a parameter
   ${ prop { (i: Int) => i must be_>(0) }.setArbitrary(positiveInts) }
   to specify a specific arbitrary instance for any parameter
   ${ prop { (s: String, j: Int) => j must be_>(0) }.setArbitrary2(positiveInts) }
   to specify all the arbitrary instances
   ${ prop { (i:Int, j: Int) => i+j must be_>(0) }.setArbitraries(positiveInts, positiveInts) }

   Gen
   to specify a specific generator for a parameter
   ${ prop { (i: Int) => i must be_>(0) }.setGen(positiveInts.arbitrary) }
   to specify a specific generator for any parameter
   ${ prop { (s: String, j: Int) => j must be_>(0) }.setGen2(positiveInts.arbitrary) }
   to specify all the generators
   ${ prop { (i:Int, j: Int) => i+j must be_>(0) }.setGens(positiveInts.arbitrary, positiveInts.arbitrary) }

   Shrink
   to specify a specific shrink instance for a parameter
   ${ prop { (i: Int) =>  i === i }.setShrink(shrinkInts) }
   to specify a specific shrink instance for any parameter
   ${ prop { (s: String, j: Int) => true }.setShrink2(shrinkInts) }
   to specify all the shrink instances
   ${ prop { (i:Int, j: Int) =>  i === i }.setShrinks(shrinkInts, shrinkInts) }

   Pretty
   to specify a specific pretty instance for a parameter
   ${ prop { (i: Int) =>  i === i }.setPretty(prettyInts) }
   ${ prop { (i: Int) =>  i === i }.pretty(_.toString) }
   to specify a specific pretty instance for any parameter
   ${ prop { (s: String, j: Int) => true }.setPretty2(prettyInts) }
   ${ prop { (s: String, j: Int) => true }.pretty2(_.toString) }
   to specify all the pretty instances
   ${ prop { (i: String, j: Int) => i === i }.setPretties(prettyStrings, prettyInts) }
   ${ prop { (i: String, j: Int) => i === i }.pretties(_.toString, _.toString) }
   to specify the pretty for collected data
   ${ prop { (i: String, j: Int) => i === i }.collectAll.prettyFreqMap((fq: FreqMap[Set[Any]]) => fq.total.toString) }
   ${ prop { (i: String, j: Int) => i === i }.collectAll.prettyFreqMap(_.toString) }

   Collect
   to specify a specific collect function for a parameter
   ${ prop { (i: Int) =>  i === i }.collect }
   ${ prop { (i: Int) =>  i === i }.collectArg((i:Int) => math.abs(i)) }
   to specify a specific collect function for any parameter
   ${ prop { (s: String, j: Int) => true }.collect2 }
   ${ prop { (s: String, j: Int) => true }.collectArg2(math.abs) }
   to specify all the collect functions
   ${ prop { (i: Int, j: Int) =>  i === i }.collectAll }
   ${ prop { (i: Int, j: Int) =>  i === i }.collectAllArgs(math.abs, math.abs) }


 Test parameters can also be specified
   the minimum number of ok tests
   ${ prop { (i:Int) => (i > 0) ==> (i > 0) }.set(minTestsOk = 50) }
   the verbosity of a property can be turned on and off
   ${ prop { (i:Int) => (i > 0) ==> (i > 0) }.set(minTestsOk = 50).verbose }


 Features
   AsResult can be used to define properties
   shrink can be turned on / off
   data collection can be turned on / off for any of the argument
   a Pretty instance can be set for any argument
   data can be cleaned up before/after execution
   parameters can be passed from the command line

 Reporting
   Labelled properties are reported
   Collected data is reported

"""


  val positiveInts = Arbitrary(Gen.choose(1, 5))

  val shrinkInts = Shrink[Int](x => Stream.empty)

  val prettyInts = (i: Int) => Pretty(_ => i.toString)
  val prettyStrings = (s: String) => Pretty(_ => s)

}


