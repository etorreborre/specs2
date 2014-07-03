package org.specs2
package guide

import scalaz.concurrent.Task
import scalaz.stream._

object CustomOutput extends UserGuidePage { def is = s2"""

You can implement your own reporting of specs2 specifications:

 - using the `Notifier` trait which acts like a listener
 - using a `Printer` which gives you more flexibility for reporting exactly what you want

## Notifier

The ${fullName[org.specs2.reporter.Notifier]} trait can be used to report execution events. It notifies of the following:

 * specification start: the beginning of a specification, with its name
 * specification end: the end of a specification, with its name
 * context start: the beginning of a sub-level when the specification is seen as a tree or Fragments
 * context end: the end of a sub-level when the specification is seen as a tree or Fragments
 * text: any Text fragment that needs to be displayed
 * example start
 * example result: success / failure / error / skipped / pending

All those notifications come with a location (to trace back to the originating fragment in the Specification) and a duration when relevant (i.e. for examples and actions).

You can then using the `notifier` argument to pass the name of your custom notifier:
```
sbt> testOnly *BinarySpec* -- notifier org.acme.reporting.FtpNotifier
```

## Printer

The ${fullName[org.specs2.reporter.Printer]} trait defines how to output each fragment of the specification. The only method to implement is:
```
def fold(env: Env, spec: SpecStructure): Fold[Fragment]
```

So what you need to create is a `Fold` over the executing specification. What is it? A `Fold` is composed of 5 operations:${snippet{
trait Fold[Fragment] {
  type S

  def prepare: Task[Unit]
  def init: S
  def sink: Sink[Task, (Fragment, S)]
  def fold: (Fragment, S) => S
  def last(s: S): Task[Unit]
}
}}

 * `prepare` is a `Task` which can "prepare" the reporting, like creating a directory
 * `init` is an initial state of type `S`. By using some state you can accumulate information about the execution of the whole specification
 * `sink` is a scalaz-stream `Sink` which can output each `Fragment` and possibly the current state
 * `fold` is the function calculating the next state based on the current `Fragment` and the previous state
 * `last` take the last state and returns a `Task` doing the last action like reporting the final statistics

Once you've defined your `Printer` trait you can use the `printer` argument like so:
```
sbt> testOnly *BinarySpec* -- printer org.acme.reporting.LatexPrinter
```
"""
}

