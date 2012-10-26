package org.specs2
package execute

import matcher.{ResultMatchers, DataTables}

class ResultSpec extends Specification with DataTables with ResultMatchers { def is =
                                                                                                                        """
Results are the outcome of some execution. There are several kinds of Results, all having a message describing them
more precisely:

  * Success: everything is ok
  * Failure: an expectation is not met
  * Error: something completely unexpected happened
  * Skipped: the user decided to skip the execution for some reason
  * Pending: the user decided that the execution was not yet implemented
                                                                                                                        """^
                                                                                                                        p^
  "Results can be combined with and"                                                                                    ^
  { (success1 and success2) must_== Success("s1 and s2") }                                                              ^
  { (success1 and success1) must_== Success("s1") }                                                                     ^
  { (success1 and failure1) must_== failure1 }                                                                          ^
  { (success1 and error1)   must_== error1 }                                                                            ^
  { (success1 and skipped1) must_== success1 }                                                                          ^
  { (skipped1 and success1) must_== success1 }                                                                          ^
  { (failure1 and success1) must_== failure1 }                                                                          ^
  { (failure1 and failure2) must_== failure1 }                                                                          ^
  { (failure1 and error1)   must_== failure1 }                                                                          ^
  { (error1   and failure1) must_== error1 }                                                                            ^
  { (error1   and failure1) must_== error1 }                                                                            ^
  { (error1   and failure1) must_== error1 }                                                                            ^
  { (error1   and failure1) must_== error1 }                                                                            ^
  "the expectationsNb must be ok"                                                                                       ^
    { (success1 and success2).expectationsNb must_== 2 }                                                                ^
    { (success1 and failure1).expectationsNb must_== 2 }                                                                ^
    { (success1 and error1)  .expectationsNb must_== 2 }                                                                ^
    { (success1 and skipped1).expectationsNb must_== 2 }                                                                ^
    { (failure1 and success1).expectationsNb must_== 1 }                                                                ^
    { (failure1 and failure2).expectationsNb must_== 1 }                                                                ^
    { (failure1 and error1)  .expectationsNb must_== 1 }                                                                ^
    { (error1   and success1).expectationsNb must_== 1 }                                                                ^bt^
    "the expected message must be ok"                                                                                   ^
    { (success1_1 and success2_1).expected must_== "exp1; exp2" }                                                       ^
    { (success1_1 and failure1_1).expected must_== "exp1; exp1" }                                                       ^
    { (success1_1 and error1)    .expected must_== "" }                                                                 ^
    { (success1_1 and skipped1_1).expected must_== "exp1; exp1" }                                                       ^
    { (failure1_1 and success1_1).expected must_== "exp1" }                                                             ^
    { (failure1_1 and failure2_1).expected must_== "exp1" }                                                             ^
    { (failure1_1 and error1)    .expected must_== "exp1" }                                                             ^
    { (error1   and success1_1)  .expected must_== "" }                                                                 ^
                                                                                                                        endp^
  "Results can be combined with or"                                                                                     ^
  { (success1 or success2) must_== Success("s1") }                                                                      ^
  { (success1 or failure1) must_== success1 }                                                                           ^
  { (success1 or skipped1) must_== success1 }                                                                           ^
  { (skipped1 or success1) must_== success1 }                                                                           ^
  { (failure1 or success1) must_== Success("f1 and s1") }                                                               ^
  { (success1 or failure1) must_== Success("s1") }                                                                      ^
  { (failure1 or failure2) must_== Failure("f1 and f2") }                                                               ^
  { (failure1 or error1)   must_== failure1 }                                                                           ^
  { (skipped1 or failure1) must_== failure1 }                                                                           ^
  "the expectationsNb must be ok"                                                                                       ^
   { (success1 or success2).expectationsNb must_== 2 }                                                                  ^
   { (success1 or failure1).expectationsNb must_== 2 }                                                                  ^
   { (success1 or skipped1).expectationsNb must_== 2 }                                                                  ^
   { (failure1 or success1).expectationsNb must_== 2 }                                                                  ^
   { (success1 or failure1).expectationsNb must_== 2 }                                                                  ^
   { (skipped1 or success1).expectationsNb must_== 1 }                                                                  ^
   { (skipped1 or failure1).expectationsNb must_== 1 }                                                                  ^
   { (failure1 or failure2).expectationsNb must_== 2 }                                                                  ^
   { (failure1 or error1)  .expectationsNb must_== 2 }                                                                  ^
  "results have methods to know their status: isSuccess, isPending, ..."                                                ! statuses^
  "A result message can be updated or mapped"                                                                           ^
  { success1.updateMessage("ok").message must_== "ok" }                                                                 ^
  { success1.mapMessage(_.capitalize).message must_== "S1" }                                                            ^ end^
  "A result expected can be updated or mapped"                                                                          ^
  { success1.updateExpected("ok").expected must_== "ok" }                                                               ^
  { Success("s1", "s1").mapExpected(_.capitalize).expected must_== "S1" }                                               ^
                                                                                                                        end^
  "Boolean values can also be combined as if they were results"                                                         ^
  { (true: Result) }                                                                                                    ^
  { true and true }                                                                                                     ^
  { (true and false) must beFailing }                                                                                   ^
  "A match result can be evaluated only when a boolean condition is satisfied"                                          ^
  { ((1 must_== 2): Result).when(false) }                                                                               ^
  "A match result can be evaluated only unless a boolean condition is satisfied"                                        ^
  { ((1 must_== 2): Result).unless(true) }                                                                              ^
  "A match result can be evaluated if and only if a boolean condition is satisfied"                                     ^
  { ((1 must_== 2): Result).iff(false) }                                                                                ^
  { ((1 must_== 1): Result).iff(true) }                                                                                 ^
                                                                                                                        end

  def statuses =
  "result" | "isSuccess" | "isFailure" | "isError" | "isSkipped" | "isPending" |>
  success1 ! true        ! false       ! false     ! false       ! false       |
  failure1 ! false       ! true        ! false     ! false       ! false       |
  error1   ! false       ! false       ! true      ! false       ! false       |
  skipped1 ! false       ! false       ! false     ! true        ! false       |
  pending1 ! false       ! false       ! false     ! false       ! true        | { (r, s, f, e, sk, p) =>
    (r.isSuccess, r.isFailure, r.isError, r.isSkipped, r.isPending) must_== (s, f, e, sk, p)
  }

  val success1: Result = Success("s1")
  val success2 = Success("s2")                                                                                          
  val failure1 = Failure("f1")                                                                                          
  val failure2 = Failure("f2")
  val error1   = Error("e1")
  val skipped1 = Skipped("sk1")
  val pending1 = Pending("p1")

  val success1_1: Result = Success("s1", "exp1")
  val success2_1 = Success("s2", "exp2")
  val failure1_1 = Failure("f1", "exp1")
  val failure2_1 = Failure("f2", "exp2")
  val skipped1_1 = Skipped("sk1", "exp1")
}