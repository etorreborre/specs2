package user
package specification

import org.specs2.Specification
import org.specs2.execute._

class UserExpectations extends Specification {
 def is = s2" $failure1 "

 // used in the MatcherSpec
 def failure1 = ("1"  must be_<(0) ^^ ((_: String).toInt)).toResult.asInstanceOf[Failure]
}
