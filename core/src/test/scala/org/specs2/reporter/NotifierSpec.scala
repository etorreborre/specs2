package org.specs2
package reporter

import main._
import control._
import matcher.ControlMatchers._
import execute.{Result, AsResult, Details}
import io.StringOutput
import specification.core.{EnvFixture, Env}
import specification.FixtureExample

class NotifierSpec extends Specification with EnvFixture { def is = s2"""

 Run a mutable spec with a Notifier $a1

"""

  def a1 = { env: Env =>
    val spec = new NotifierSpec1
    val env1 = env.copy(arguments = Arguments("notifier"))
    val notifier = new TestNotifier
    Reporter.report(env1, List(NotifierPrinter.printer(notifier)))(spec.structure(env)).runOption
    notifier.messages.mkString("\n") must_==
    List(
      "[start  ] NotifierSpec1",
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
}

class NotifierSpec1 extends org.specs2.mutable.Specification {
  "group1" >> {
    "ex1" >> ok
    "ex2" >> ok
  }
  "group2" >> {
    "ex3" >> ok
    "ex4" >> ko
  }
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
}

class ConsoleNotifier extends Notifier {
  def specStart(title: String, location: String)                                                                      = Console.println(s"[start  ] $title")
  def specEnd(title: String, location: String)                                                                        = Console.println(s"[end    ] $title")
  def contextStart(text: String, location: String)                                                                    = Console.println(s"[open   ] $text")
  def contextEnd(text: String, location: String)                                                                      = Console.println(s"[close  ] $text")
  def text(text: String, location: String)                                                                            = Console.println(s"[text   ] $text")
  def exampleStarted(name: String, location: String)                                                                  = Console.println(s"[example] $name")
  def exampleSuccess(name: String, duration: Long)                                                                    = Console.println(s"[success] $name")
  def exampleFailure(name: String, message: String, location: String, f: Throwable, details: Details, duration: Long) = Console.println(s"[failure] $name $message")
  def exampleError  (name: String, message: String, location: String, f: Throwable, duration: Long)                   = Console.println(s"[error  ] $name $message")
  def exampleSkipped(name: String, message: String, location: String, duration: Long)                                 = Console.println(s"[skipped] $name $message")
  def examplePending(name: String, message: String, location: String, duration: Long)                                 = Console.println(s"[pending] $name $message")
}
