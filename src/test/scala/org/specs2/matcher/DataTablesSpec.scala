package org.specs2
package matcher

class DataTablesSpec extends SpecificationWithJUnit with DataTables { def is = 
                                                                                          """
  DataTables are useful to specify lots of examples varying just by a few values.
                                                                                          """^
"  A simple table can be declared to specify the addition on Integers"                    ! e1^
"  A table is not executed if there is no 'play' sign |> declared on it"                  ! e2^
"  If there are failures on rows they must be reported"                                   ! e3^
"  If there is an exception on any row, it will stop the example"                         ! e4^
"  If the first value is a string, !! can be used as a cell separator"                    ! e5^
                                                                                          end

  def e1 =
	  "a"   | "b" | "c" |
	   2    !  2  !  4  |
	   1    !  1  !  2  |> { (a, b, c) =>  a + b must_== c }
	   
  def e2 = // if the table was executed, it would go "boom"
    "a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  2  | { (a, b, c) => error("boom"); a + b must_== c }

  def e3 = 
   ("a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  3  |> { (a, b, c) => a + b must_== c }).isSuccess must beFalse

  def e4 = 
   ("a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  3  |> { (a, b, c) => error("boom"); a + b must_== c }) must throwAn[Exception]

  def e5 =
    "a"     |  "b"      | "c"             |
    "Hello" !! "world"  !  "Hello world"  |> { (a, b, c) =>  a +" "+b must_== c }
}