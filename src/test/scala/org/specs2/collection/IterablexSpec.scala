package org.specs2
package collection
import mutable.Specification
import Listx._
import org.scalacheck.Prop
import Iterablex._
import matcher.ScalaCheckMatchers

class IterablexSpec extends Specification with IterableData {

  "Specification for Iterables extensions".title

  "The sameElementsAs function returns true" >> {
    "if 2 lists of lists contain the same elements in a different order" >> {
      List(List(1), List(2, 3)).sameElementsAs(List(List(3, 2), List(1)))
    }
    "if deeply nested lists have the same elements but in a different order" >> {
      List(1, List(2, 3, List(4)), 5).sameElementsAs(List(5, List(List(4), 2, 3), 1))
    }
    "when comparing xml nodes in a different order" >> {
      <a> <b/> <c/> </a>.child.sameElementsAs(<a> <c/> <b/> </a>.child)
    }
    "for 2 iterables created with same elements in a different order" >> {
      implicit val iterables = sameIterables
      Prop.forAll { t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t 
        i1.sameElementsAs(i2)
      }
    }
    "for 2 iterables created with same elements in a different order, even with different types like Stream and List" >> {
      implicit val iterables = sameIterablesOfDifferentTypes
      Prop.forAll { t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t
        i1.sameElementsAs(i2)
      }
    }
  }

  "The containsInOrder function should" >> {
    "check that some values are contained inside an Iterable, in the same order" in {
      List(1, 2, 3).containsInOrder(1, 3)
    }
    "detect if some values are contained inside an Iterable in a different order" in {
      ! List(1, 2, 3).containsInOrder(2, 1)
    }
  }

  "toDeepString uses recursively the toString method to display iterables in brackets" in
  { List(List(1, 2), 3, List(4, 5)).toDeepString must_== "[[1, 2], 3, [4, 5]]" }

  "mapFirst maps the first element with a function if it exists" in
  { Seq(1, 2).mapFirst(_ + 1) must_== Seq(2, 2) }

  "mapLast maps the last element with a function if it exists" in
  { Seq(1, 2).mapLast(_ + 1) must_== Seq(1, 3) }

}

import org.scalacheck.Arbitrary
import org.scalacheck.Gen._

trait IterableData extends ScalaCheckMatchers {
  val sameIterables: Arbitrary[(Iterable[Any], Iterable[Any])] = Arbitrary {
    for {
      i0 <- listOf(oneOf(1, 2, 3))
      i1 <- listOf(oneOf(1, 4, 5, i0))
      i2 <- listOf(oneOf(i0, i1, 2, 3))
    } yield (i2, i2.scramble)
  }                           
  val sameIterablesOfDifferentTypes: Arbitrary[(Iterable[Any], Iterable[Any])] = Arbitrary {
    listOf(oneOf(1, 2, 3, listOf(oneOf(1, 2, 3)))).map(i1 => (i1.toStream, i1.scramble.toList))
  }
}
