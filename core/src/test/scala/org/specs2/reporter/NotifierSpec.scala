package org.specs2
package reporter

import main._
import control._
import execute._
import io.StringOutput
import org.specs2.specification.{AfterAll, Tables}
import specification.core.Env
import runner._

class NotifierSpec(env: Env) extends Specification { def is = s2"""

 Run a mutable spec with a Notifier                     $a1
 Run an acceptance spec with a Notifier                 $a2
 Run a specification with decorated results             $a3
 Blocks must only be closed once even when separated    $a4
 A failure in a spec must fail the specification        $a5

"""

  def a1 = {
    val spec = new NotifierSpec1
    val env1 = env.setArguments(Arguments("notifier"))
    val notifier = new TestNotifier
    Reporter.report(env1, List(NotifierPrinter.printer(notifier)))(spec.structure(env)).runOption(env.executionEnv)
    notifier.messages.mkString("\n") must_==
    List(
      "[start  ] NotifierSpec1",
      "[step   ]",
      "[success]",
      "[open   ] group1",
      "[example] ex1",
      "[success] ex1",
      "[example] ex2",
      "[success] ex2",
      "[close  ] group1",
      "[open   ] group2",
      "[example] ex3",
      "[success] ex3",
      "[example] ex4",
      "[failure] ex4 ko",
      "[close  ] group2",
      "[end    ] NotifierSpec1").mkString("\n")
  }

  def a2 = {
    val spec = new NotifierSpec1
    NotifierRunner(new GlobalNotifier).main(Array(spec.getClass.getName))

    "the notifier is called" ==> GlobalNotifier.called
  }

  def a3 = {
    val spec = new NotifierSpecWithTables
    val env1 = env.setArguments(Arguments("notifier"))
    val notifier = new TestNotifier
    Reporter.report(env1, List(NotifierPrinter.printer(notifier)))(spec.structure(env)).runOption(env.executionEnv)

    "there is a failure for the table" ==> {
      notifier.messages must contain((m: String) => m must contain("failure"))
    }
  }

  def a4 = {
    val spec = new NotifierSpec2
    val env1 = env.setArguments(Arguments("notifier"))
    val notifier = new TestNotifier
    Reporter.report(env1, List(NotifierPrinter.printer(notifier)))(spec.structure(env)).runOption(env.executionEnv)
    notifier.messages.mkString("\n") must_==
      List(
        "[start  ] NotifierSpec2",
        "[open   ] group1",
        "[example] ex1",
        "[success] ex1",
        "[close  ] group1",
        "[open   ] group2",
        "[example] ex2",
        "[success] ex2",
        "[close  ] group2",
        "[end    ] NotifierSpec2").mkString("\n")
  }

  def a5 = {
    val spec = new NotifierSpec3
    val env1 = env.setArguments(Arguments("notifier"))
    val notifier = new TestNotifier
    Reporter.report(env1, List(NotifierPrinter.printer(notifier)))(spec.structure(env)).runOption(env.executionEnv)
    notifier.messages.mkString("\n") must_==
      List(
        "[start  ] NotifierSpec3",
        "[open   ] group1",
        "[example] ex1",
        "[success] ex1",
        "[close  ] group1",
        "[step   ]",
        "[error  ] org.specs2.specification.core.FatalExecution: boom",
        "[end    ] NotifierSpec3").mkString("\n")
  }

}

class NotifierSpecWithTables extends Specification with Tables {def is = s2"""
  a table ${
    "a" | "b" | "e"  |>
    "a" ! "b" ! "AB" | { (a, b, e) => a + b must_== e }
  }
  """
}
class NotifierSpec1 extends org.specs2.mutable.Specification {
  step("a step")
  "group1" >> {
    "ex1" >> ok
    "ex2" >> ok
  }
  "group2" >> {
    "ex3" >> ok
    "ex4" >> ko
  }
}

class NotifierSpec2 extends org.specs2.mutable.Specification {
  "group1" >> {
    "ex1" >> ok
  }
  br
  "group2" >> {
    "ex2" >> ok
  }
}

class NotifierSpec3 extends org.specs2.mutable.Specification with AfterAll {
  "group1" >> {
    "ex1" >> ok
  }
  def afterAll = sys.error("boom")
}

class TestNotifier extends Notifier with StringOutput {
  def specStart(title: String, location: String) = append(s"[start  ] $title")
  def specEnd(title: String, location: String) = append(s"[end    ] $title")
  def contextStart(text: String, location: String) = append(s"[open   ] $text")
  def contextEnd(text: String, location: String) = append(s"[close  ] $text")
  def text(text: String, location: String) = append(s"[text   ] $text")
  def exampleStarted(name: String, location: String) = append(s"[example] $name")
  def exampleSuccess(name: String, duration: Long) = append(s"[success] $name")
  def exampleFailure(name: String, message: String, location: String, f: Throwable, details: Details, duration: Long) = append(s"[failure] $name $message")
  def exampleError  (name: String, message: String, location: String, f: Throwable, duration: Long) = append(s"[error  ] $name $message")
  def exampleSkipped(name: String, message: String, location: String, duration: Long) = append(s"[skipped] $name $message")
  def examplePending(name: String, message: String, location: String, duration: Long) = append(s"[pending] $name $message")
  def stepStarted(location: String) = append(s"[step   ]")
  def stepSuccess(duration: Long) = append(s"[success]")
  def stepError(message: String, location: String, f: Throwable, duration: Long) = append(s"[error  ] $message")
}

class GlobalNotifier extends SilentNotifier {
  override def specStart(title: String, location: String) =
    GlobalNotifier.called = true
}

object GlobalNotifier {
  var called = false
}

object NotifierTest {
  def main(args: Array[String]): Unit = {
    org.specs2.runner.NotifierRunner(new org.specs2.reporter.ConsoleNotifier).main(args)
  }
}
