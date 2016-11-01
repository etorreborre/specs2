package user

import org.specs2._

class TestSpec extends Specification {
 def is = s2"""
 test1  ${ex(1)}
 test2  ${ex(2)}
    """

  def ex(i: Int) = {
    s"start t$i".pp
    Thread.sleep(200)
    s"finished t$i".pp
    ok
  }


}
