package user

import org.specs2._

class TestSpec extends Specification {
 def is = s2"""

 test1  ${ex(1)}
 test2  ${ex(2)}
 test3  ${ex(3)}
 test4  ${ex(4)}
 test5  $${ex(5)}
 test6  $${ex(6)}
 test7  $${ex(7)}
 test8  $${ex(8)}
 test9  $${ex(9)}
 test10 $${ex(10)}
 test11 $${ex(11)}
 test12 $${ex(12)}

    """

  def ex(i: Int) = {
    s"start t$i".pp
    Thread.sleep(200)
    s"finished t$i".pp
    ok
  }


}
