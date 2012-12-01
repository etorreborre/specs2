package org.specs2
package matcher

import scalaz._
import Scalaz._

object ValidationMatchersSpec extends Specification with ThrownExpectations {   def is =
  "`Success` matching mirrors `Right` matching"                                 ! {
    1.success must be zsuccess (1)
    1.right   must be right    (1)

    1.success must not be zsuccess (2)
    1.right   must not be right    (2)

    1.success must beZSuccess (1)
    1.right   must beRight    (1)

    1.success must not beZSuccess (2)
    1.right   must not beRight    (2)

    1.success must beZSuccess
    1.right   must beRight

    1.success must not beZFailure //completely blank line next or it wont compile!

    1.right   must not beLeft

    1.success must be zsuccess //completely blank line next or it wont compile!

    1.right   must be right

    1.success must not be zfailure
    1.right   must not be left

    List(1, 2).success must beZSuccess.like {
      case 1 :: _ => ok
    }
    List(1, 2).right   must beRight.like {
      case 1 :: _ => ok
    }
  }                                                                             ^
  "`Failure` matching mirrors `Left` matching"                                  ! {
    1.fail must be zfailure (1)
    1.left must be left     (1)

    1.fail must not be zfailure (2)
    1.left must not be left     (2)

    1.fail must beZFailure (1)
    1.left must beLeft     (1)

    1.fail must not beZFailure (2)
    1.left must not beLeft     (2)

    1.fail must beZFailure
    1.left must beLeft

    1.fail must not beZSuccess //completely blank line next or it wont compile!

    1.left must not beRight

    1.fail must be zfailure //completely blank line next or it wont compile!

    1.left must be left

    1.fail must not be zsuccess
    1.left must not be right

    List(1, 2).fail must beZFailure.like {
      case 1 :: _ => ok
    }
    List(1, 2).left must beLeft.like {
      case 1 :: _ => ok
    }
  }                                                                             ^
                                                                                end

}

