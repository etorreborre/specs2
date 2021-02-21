package user.reporter

import org.specs2.execute.*
import org.specs2.reporter.Notifier

class CustomNotifier extends Notifier:
  def specStart(title: String, location: String)                                                                      = Console.println(s"[start]   $title")
  def specEnd(title: String, location: String)                                                                        = Console.println(s"[end]     $title")
  def contextStart(text: String, location: String)                                                                    = Console.println(s"[open]    $text")
  def contextEnd(text: String, location: String)                                                                      = Console.println(s"[close]   $text")
  def text(text: String, location: String)                                                                            = Console.println(s"[text]    $text")
  def exampleStarted(name: String, location: String)                                                                  = Console.println(s"[example] $name")
  def exampleSuccess(name: String, duration: Long)                                                                    = Console.println(s"[success] $name")
  def exampleFailure(name: String, message: String, location: String, f: Throwable, details: Details, duration: Long) = Console.println(s"[failure] $name $message")
  def exampleError  (name: String, message: String, location: String, f: Throwable, duration: Long)                   = Console.println(s"[error]   $name $message")
  def exampleSkipped(name: String, message: String, location: String, duration: Long)                                 = Console.println(s"[skipped] $name $message")
  def examplePending(name: String, message: String, location: String, duration: Long)                                 = Console.println(s"[pending] $name $message")
  def stepStarted(location: String)                                                                                   = Console.println(s"[step]")
  def stepSuccess(duration: Long)                                                                                     = Console.println(s"[success]")
  def stepError  (message: String, location: String, f: Throwable, duration: Long)                                    = Console.println(s"[error]   $message")
