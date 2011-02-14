package org.specs2
package collection
import mutable.SpecificationWithJUnit
import Listx._
import org.scalacheck.Prop
import Iterablex._

class IterablexSpec extends SpecificationWithJUnit with IterableData {

  "Specification for Iterables extensions".title

  "A sameElementsAs function should return true" >> {
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
  "A containsInOrder function" should {
    "check that some values are contained inside an Iterable, in the same order" in {
      List(1, 2, 3).containsInOrder(1, 3)
    }
    "detect if some values are contained inside an Iterable in a different order" in {
      ! List(1, 2, 3).containsInOrder(2, 1)
    }
  }

  "toDeepString uses recursively the toString method to display iterables in brackets" in
  { List(List(1, 2), 3, List(4, 5)).toDeepString must_== "[[1, 2], 3, [4, 5]]" }

}
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Gen._
trait IterableData extends ScalaCheck {
  val sameIterables: Arbitrary[(Iterable[Any], Iterable[Any])] = Arbitrary {
    for {
      i0 <- listOf(oneOf(1, 2, 3))
      i1 <- listOf(oneOf(1, 4, 5, i0))
      i2 <- listOf(oneOf(i0, i1, 2, 3))
    } yield (i2, i2.scramble)
  }                           
  val sameIterablesOfDifferentTypes: Arbitrary[(Iterable[Any], Iterable[Any])] = Arbitrary {
    for {
      i1 <- listOf(oneOf(1, 2, 3, listOf(oneOf(1, 2, 3))))    
    } yield (i1.toStream, i1.scramble.toList)
  }
}
