package user

import org.specs2._

class TestSpec extends Specification with ScalaCheck { def is = s2"""

 e1 $e1

"""

  def e1 = prop { (n: Int) =>
    n ==== n
  }

}

import org.specs2.specification.core._
import scala.concurrent._

class TestMutableSpec(using ec: ExecutionContext) extends org.specs2.mutable.Specification:
  def asyncStep[T : AsExecution](t: => T) =
    addFragment(Fragment(NoText, AsExecution[T].execute(t).join))

  asyncStep(Future { println("start here"); true })

  "e1" >> { println("e1 "+Thread.currentThread.getName); success }
  "e2" >> { println("e2 "+Thread.currentThread.getName); success }
  "e3" >> { println("e3 "+Thread.currentThread.getName); success }


// class TestMutableSpec extends org.specs2.mutable.Specification with AfterEachAsync:
//   var i = 1
//   def afterAsync: Any =
//     Thread.sleep(500)
//     println("after example " + i + " " + Thread.currentThread.getName)
//     i = i + 1

//   "e1" >> { println("e1 "+Thread.currentThread.getName); success }
//   "e2" >> { println("e2 "+Thread.currentThread.getName); success }
//   "e3" >> { println("e3 "+Thread.currentThread.getName); success }

// import org.specs2.specification.create._
// import org.specs2.control.producer.Producer._

// trait AfterEachAsync extends org.specs2.mutable.Specification:
//   override def map(fs: =>Fragments): Fragments =
//     super.map(fs.flatMap { f =>
//       if Fragment.isExample(f) then
//         emitAsync(List(f, step(afterAsync)))
//       else
//         emitAsync(List(f))
//     })

//   protected def afterAsync: Any
