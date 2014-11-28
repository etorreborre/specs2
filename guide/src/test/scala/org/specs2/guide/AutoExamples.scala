package org.specs2
package guide

import collection.Seqx._

object AutoExamples extends UserGuidePage { def is = "Auto-examples".title ^ s2"""

When you want to specify an API, most of your examples are self-describing:${snippet{
class SeqSpecification extends mutable.Specification {
  "updateLast modifies the last element of a Seq".p
   "when the collection has 1 element"  >> { Seq(1).updateLast(_ + 1)     must_== Seq(2) }
   "when the collection has 2 elements" >> { Seq(1, 2).updateLast(_ + 1)  must_== Seq(1, 3) }
   "when the collection is empty"       >> { Seq[Int]().updateLast(_ + 1) must_== Seq[Int]() }
}
}}

It is a bit redundant to provide a textual description for these 3 examples because the code is pretty clear and simple. In this situation you can use the `eg` operator to create an example where the description will be the code itself:${snippet{
class SeqSpecification extends mutable.Specification {
  "updateLast modifies the last element of a Seq".p
  eg { Seq(1).updateLast(_ + 1)     must_== Seq(2)     }
  eg { Seq(1, 2).updateLast(_ + 1)  must_== Seq(1, 3)  }
  eg { Seq[Int]().updateLast(_ + 1) must_== Seq[Int]() }
}
}}

This prints:
```
[info] updateLast modifies the last element of a Seq
[info]  Seq(1).updateLast(_ + 1)     must_== Seq(2)
[info]  Seq(1, 2).updateLast(_ + 1)  must_== Seq(1, 3)
[info]  Seq[Int]().updateLast(_ + 1) must_== Seq[Int]()
```

#### In an acceptance specification

Acceptance specifications are using interpolated strings so you can directly write:${snippet{
class SeqSpecification extends Specification { def is = s2"""
  updateLast modifies the last element of a Seq
  ${ Seq(1).updateLast(_ + 1)     must_== Seq(2)     }
  ${ Seq(1, 2).updateLast(_ + 1)  must_== Seq(1, 3)  }
  ${ Seq[Int]().updateLast(_ + 1) must_== Seq[Int]() }
"""
}
}}

There is a huge gotcha though! Each of these expressions needs an implicit conversion to be included in the interpolated spec. And in Scala, if you have a block of code returning a value of type `T`, ***only the last expression of the block is converted***. This means that if there is a statement in the block that throws an exception, this exception won't be caught and the whole specification will fail to be instantiated! So if you want to use blocks as auto-examples you should better wrap them with an `eg` call:${snippet{
class SeqSpecification extends Specification { def is = s2"""
  This is a problematic specification
  ${ sys.error("ouch, this one is going to blow up the spec"); Seq(1).updateLast(_ + 1) must_== Seq(2) }
  ${ eg { sys.error("it's ok, this one is well protected");    Seq(1).updateLast(_ + 1) must_== Seq(2) } }
"""
}
}}

"""
}
