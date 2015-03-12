package org.specs2
package matcher

import execute._

class StandardMatchResultsSpec extends Specification { def is = s2"""

 Some match results are predefined: ok and ko
   ok can be created with a message $ok1
   ko can be created with a message $ko1
   ko has a location                $ko2

"""

  def ok1 =
    ok("this is ok").message     === "this is ok"

  def ko1 =
    ko("this is ko").message     === "this is ko"

  def ko2 =
    ko.toResult must beLike { case f: Failure => f.stackTrace must not beEmpty }
  
}


