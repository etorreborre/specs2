package user

import org.specs2._
import org.specs2.concurrent.ExecutionEnv

class TestSpec(implicit ee: ExecutionEnv) extends Specification {
 def is = s2"""
 test1  ${ex(1)}
 test2  ${ex(2)}
 ${step(println("barrier"))}
 test3  ${ex(3)}
 test4  ${ex(4)}
 test5  $${ex(5)}
 test6  $${ex(6)}
 test7  $${ex(7)}

 test $$test
    """

  def ex(i: Int) = {
    s"start t$i".pp
    Thread.sleep((100*(1 + scala.util.Random.nextInt(5))).toLong)
    s"finished t$i".pp
    ok
  }


  def test = {
    import scalaz._, Scalaz._
    import org.specs2.control._, eff._, all._
    import org.specs2.control.eff.syntax.all._
    val asyncInt = AsyncFutureInterpreter.create
    import asyncInt._
    import scala.concurrent._, duration._

    import producer._, Producer._
    type S = Fx.fx2[Safe, Async]

    val rs =
      Producer.sequence[S, Async, String](4)(emit[S, Int](List(200, 300, 50)).map { i =>
        asyncFork {
          ("start "+i).pp
          Thread.sleep(i.toLong)
          ("finished"+i).pp
        }
      })

    rs.run.runSafe.runAsyncFuture must be_==("").awaitFor(5.seconds)
  }

}
