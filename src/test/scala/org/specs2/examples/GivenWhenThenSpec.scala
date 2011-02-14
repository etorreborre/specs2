package org.specs2
package examples

class GivenWhenThenSpec extends SpecificationWithJUnit { def is = sequential                                            ^
                                                                                                                        """
This specification shows how to write examples in a Given/When/Then style using a mutable
system.

Note that the arguments specify that the specification is sequential because each example has
to be executed in order.
                                                                                                                        """^
                                                                                                                        p^
  "Given that the customer buys 3 books at 10 dollars each"                                                             ! s1.buyBook^
  "Given that the customer buys 1 book at 20 dollars"                                                                   ! s1.buyBook^
  "When he checks out"                                                                                                  ! s1.checkout^
  "Then the total price must be 50 dollars"                                                                             ! s1.total^
                                                                                                                        end

  case object s1 {
    val BuyBooks = ".* buys (\\d+) book.? at (\\d+) .*".r     // a regular expression for extracting the quantity and price
    val TotalBooks = ".* must be (\\d+) .*".r                 // a regular expression for extracting the total price
    val books: scala.collection.mutable.Map[Int, Int] = new scala.collection.mutable.HashMap[Int, Int]()

    def buyBook = (s: String) => {
      val BuyBooks(qty, price) = s
      books += qty.toInt -> price.toInt
      success
    }
    def checkout = books must not be empty
    def total = (s: String) => {
      val TotalBooks(total) = s
      books.foldLeft(0)((res, cur) => res + cur._1 * cur._2) must_== total.toInt
    }
  }
}