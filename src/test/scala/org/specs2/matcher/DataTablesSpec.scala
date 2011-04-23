package org.specs2
package matcher

class DataTablesSpec extends Specification with DataTables with ResultMatchers { def is =
                                                                                                                        """
  DataTables are useful to specify lots of examples varying just by a few values.
                                                                                                                        """^
  "A simple table can be declared to specify the addition on Integers"                                                  ! e1^
  "A table is not executed if there is no 'play' sign `|>` declared on it"                                              ! e2^
  "If there are failures on rows they must be reported"                                                                 ! e3^
  "If there is an exception on any row, it will not stop the example"                                                   ! e4^
  "If the first value is a string, !! can be used as a cell separator"                                                  ! e5^
  "!! can be used as a cell separator with any type"                                                                    ! e5_1^
  "A table can be built with just one column"                                                                           ! e6^
  "A table must work with values of different subtypes of the first row"                                                ! e7^
                                                                                                                        end

  def e1 =
	  "a"   | "b" | "c" |>
	   2    !  2  !  4  |
	   1    !  1  !  2  | { (a, b, c) =>  a + b must_== c }
	   
  def e2 = // if the table was executed, it would go "boom"
    "a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  2  | { (a, b, c) => error("boom"); a + b must_== c }

  def e3 = 
   ("a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  3  |> { (a, b, c) => a + b must_== c }) must beFailing

  def e4 = 
   ("a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  3  |> { (a, b, c) => error("boom"); a + b must_== c }) must beError

  def e5 =
    "a"     |  "b"      | "c"             |
    "Hello" !! "world"  !  "Hello world"  |> { (a, b, c) =>  a +" "+b must_== c }

  def e5_1 =
    "a"           ||  "b"      | "c"            |
    ("Hello":Any) !! "world"   !  "Hello world" |
    1             !! "world"   !  "1 world"     |> { (a, b, c) =>  a +" "+b must_== c }

  def e6 =
	  "a"   |
	   2    |
	   1    |> { (a) =>  a must be_>=(0) }


  def e7 =
	  "a"         | "b"       |>
	  (0: Any)    ! "0"       |
	  (List("a")) ! "List(a)" | { (a, b) =>  a.toString must_== b }

}
