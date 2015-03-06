package org.specs2
package guide

import main._

object ConsoleOutput extends UserGuidePage { def is = "Console output".title ^ s2"""

You will get a "console" output whenever you run a specification from ${"sbt" ~/ Runners}, ${"Maven, Gradle" ~/ OtherBuildTools} or a ${"shell session" ~/ RunInShell}. There are arguments you can use to change the output:

  Name               | Value format            | Comments
 ------------------- | ----------------------- | ------------------------------------------------------------------------
 `xonly`             | boolean                 | only report failures and errors
 `showonly`          | String                  | only report some examples based on their status
 `failtrace`         | boolean                 | show a stack trace for failures
 `fullstacktrace`    | boolean                 | show a full stack trace
 `tracefilter`       | regexp-csv/regexp-csv   | comma-separated include patterns separated by `/` with exclude patterns
 `smartdiffs`        | see below               | define the parameters for the `SmartDiffs` instance for differences
 `diffsclass`        | class name              | use a specific instance of the `Diffs` trait
 `nocolor`           | boolean                 | do not use colors
 `colors`            | map                     | use different colors
 `colorsclass`       | class name              | use a specific instance of the `Colors` trait
 `showtimes`         | boolean                 | show execution times for examples
 `indentation`       | int                     | number of spaces to use to indent nested examples (default = 2)

Some of these arguments deserve further explanations.

### Show only

You can decide what you want to _show_ in the console by using the `showonly` arguments and the following flags:

  Flag | Description
 ----- | ------------
  `+`  | successful example
  `!`  | error example
  `o`  | skipped example
  `*`  | pending example
  `-`  | text
  `1`  | statistics

For example if you just want to show text and failures you can use `showonly -x`. And the `xonly` argument is actually a shortcut for `showonly x!`.

### StackTraceFilter

The `tracefilter` argument uses include/exclude patterns to define an instance of the `org.specs2.control.StackTraceFilter` trait which will filter stacktraces. By default the `DefaultStackTraceFilter` filter will exclude lines matching the following packages:

 * `org.specs2`
 * `scalaz\\.`
 * `scala\\.`, `java\\.`
 * `sbt\\.`, `com.intellij`, `org.eclipse.jdt`, `org.junit`

If this is not what you want, you can either use the `tracefilter` argument with other patterns. For example `tracefilter com.acme\com.acme.impl` will only keep traces of classes in the `com.acme` package but will reject the ones in `com.acme.impl`.

Note also that the default filter also truncates the stacktrace in the middle if it is bigger than 1000 lines to avoid reports being filled by out-of-control stacktraces. If you still want to see those lines you can re-run with the `fullstacktrace` argument.

### Diffs

When using the equality matcher $specs2 tries to display the difference between the expected and the actual values using a class: `org.specs2.main.SmartDiffs`. There are several parameters for that class which you can specify from the command line as:
```
sbt> test-only -- smartdiffs show,separators,triggerSize,shortenSize,diffRatio,full
```

 Parameter          | Description
 ------------------ | -----------
 `show`             | will not show anything (default is true)
 `separators`       | allows to change the separators used to show the differences (default is "[]")
 `triggerSize`      | controls the size above which the differences must be shown (default is 20)
 `shortenSize`      | controls the number of characters to display around each difference (default is 5)
 `diffRatio`        | percentage of differences above which the differences must not be shown (default is 30)
 `full`             | displays the full original expected and actual strings
 `seqTriggerSize`   | the minimum size to compute differences on Seq, Set and Maps
 `seqMaxSize`       | the maximum size to compute differences on Seq, Set and Maps


You can also specify your own enhanced algorithm for displaying the difference by providing an instance of the `${fullName[Diffs]}` trait:
```
trait Diffs {
  /** @return true if the differences must be shown */
  def show: Boolean
  /** @return true if the differences must be shown for 2 different values */
  def show(actual: Any, expected: Any): Boolean
  /** @return true if the differences must be shown for 2 different sequences of values */
  def showSeq(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): Boolean
  /** @return true if the differences must be shown for 2 different maps */
  def showMap(actual: Map[Any, Any], expected: Map[Any, Any]): Boolean
  /** @return the diffs */
  def showDiffs(actual: Any, expected: Any): (String, String)
  /** @return the diffs for sequences with missing / added values  */
  def showSeqDiffs(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): (Seq[String], Seq[String])
  /** @return the diffs for sequences with missing / added values  */
  def showMapDiffs(actual: Map[Any, Any], expected: Map[Any, Any]): (Seq[String], Seq[String], Seq[String])
  /** @return true if the full strings must also be shown */
  def showFull: Boolean
}
```

### Colors

By default, the reporting will output colors. If you're running on windows you might either:

 * use the [following tip](http://www.marioawad.com/2010/11/16/ansi-command-line-colors-under-windows) to install colors in the DOS console
 * or pass `nocolor` as a command line argument

It is possible to set colors by passing the `colors` argument. This argument must be a list of `key:value` pairs (comma-separated) where keys are taken from this table:

Property  | Default value |
--------- | ------------- |
`text`    |  white        |
`success` |  green        |
`failure` |  yellow       |
`error`   |  red          |
`pending` |  cyan         |
`skipped` |  magenta      |
`stats`   |  cyan         |

All the available colors are listed here, with their corresponding abbreviation which you can use to refer to them as well:

 Color   | Abbreviation |
 ------  | ------------ |
 white   | w            |
 green   | g            |
 yellow  | y            |
 red     | r            |
 blue    | be           |
 cyan    | c            |
 black   | bk           |
 magenta | m            |



For example you can pass on the command line:

 `colors text:blue,failure:magenta`

to have the text colored in blue and the failures in Magenta.

If the `colors` option contains `whitebg` then the default colors are considered to be [`InvertedColors`](${API_DIR}/index.html#org.specs2.text.Colors):

Property  | Default value |
--------- | ------------  |
`text`    |  black        |
`success` |  green        |
`failure` |  magenta      |
`error`   |  red          |
`pending` |  blue         |
`skipped` |  cyan         |
`stats`   |  blue         |

You can also change the color scheme that's being used on the console by implementing your own [`org.specs2.text.Colors`](${API_DIR}/index.html#org.specs2.text.Colors) trait or override values in the existing `ConsoleColors` class and pass this class to the `colorsclass` argument. For example if you want to output magenta everywhere yellow is used you can write:
```
class MyColors extends org.specs2.text.ConsoleColors {
  override val failureColor = magenta
}
```

and invoke `colorsclass org.acme.MyColors`

Finally note that the the color support for sbt on Windows is a bit tricky. You need to follow the instructions [here](http://www.marioawad.com/2010/11/16/ansi-command-line-colors-under-windows) then add to your script launching sbt:

```
-Djline.terminal=jline.UnsupportedTerminal
```

"""
}

