package org.specs2
package reporter

import org.specs2.execute.Details

/**
 * This trait can be used for any event concerning the execution of examples
 * seen as a Tree of Fragments.
 *
 * A pair of contextStart/contextEnd calls delimits a sequence of children in that tree.
 */
trait Notifier {
  def specStart(title: String, location: String)
  def specEnd(title: String, location: String)
  def contextStart(text: String, location: String)
  def contextEnd(text: String, location: String)
  def text(text: String, location: String)
  def exampleStarted(name: String, location: String)
  def exampleSuccess(name: String, duration: Long)
  def exampleFailure(name: String, message: String, location: String, f: Throwable, details: Details, duration: Long)
  def exampleError  (name: String, message: String, location: String, f: Throwable, duration: Long)
  def exampleSkipped(name: String, message: String, location: String, duration: Long)
  def examplePending(name: String, message: String, location: String, duration: Long)
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

