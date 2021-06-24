package examples

import org.specs2.*
import org.specs2.main.*
import extras.*

/**
 * This specification shows how to create examples using the "acceptance" style
 */
class HelloWorldSpec extends Specification:
  def is = s2"""

 This is a specification to check the 'Hello world' string

 The 'Hello world' string should
   contain 11 characters $e1
   start with 'Hello' $e2
   end with 'world' $e3
""".addExampleTimes


  def e1 = "Hello world" must haveSize(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")

import org.specs2.specification.*
import org.specs2.specification.core.*
import org.specs2.execute.*
import org.specs2.time.*


object extras:
  extension (fs: Fragments)
    def addExampleTimes: Fragments =
      fs.map {
        case f if Fragment.isExample(f) =>
          f.updateResult { r =>
            val (result, timer) = withTimer(ResultExecution.execute(AsResult(r)))
            result.updateExpected(s"""Execution time for "${f.description.show}": ${timer.time}""")
          }
        case other => other
      }

  /** measure the execution time of a piece of code */
  def withTimer[T](t: =>T): (T, SimpleTimer) = {
    val timer = (new SimpleTimer).start
    val result = t
    (result, timer.stop)
  }


trait Timed extends AroundEach:
  def around[T : AsResult](t: =>T): Result = {
    // use `ResultExecution.execute` to catch possible exceptions
    val (result, timer) = withTimer(ResultExecution.execute(AsResult(t)))
    // update the result with a piece of text which will be displayed in the console
    result.updateExpected("Execution time: "+timer.time)
  }
  /** measure the execution time of a piece of code */
  def withTimer[T](t: =>T): (T, SimpleTimer) = {
    val timer = (new SimpleTimer).start
    val result = t
    (result, timer.stop)
  }
