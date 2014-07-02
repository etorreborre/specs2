package org.specs2
package guide

import main._

object ConsoleOutput extends UserGuidePage { def is = s2"""

You will get a "console" output whenever you run a specification from ${"sbt" ~ Runners}, ${"Maven, Gradle" ~ OtherBuildTools} or a ${"shell session" ~ RunInShell}. There are arguments you can use to change the output:

  Name               | Value format            | Comments
 ------------------- | ----------------------- | ------------------------------------------------------------------------
 `xonly`             | boolean                 | only report failures and errors
 `showonly`          | String                  | only report some examples based on their status
 `failtrace`         | boolean                 | show a stack trace for failures
 `fullstacktrace`    | boolean                 | show a full stack trace
 `tracefilter`       | regexp-csv/regexp-csv   | comma-separated include patterns separated by `/` with exclude patterns
 `smartdiffs`        | see below               |
 `diffsclass`        | class name              |
 `nocolor`           | boolean                 | do not use colors
 `colors`            | map                     | use different colors
 `showtimes`         | boolean                 | show execution times for examples

Some of these arguments deserve further explanations.

### `showonly`

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

When using the equality matcher specs2 tries to display the difference between the expected and the actual values using a class: `org.specs2.main.SmartDiffs`. There are several parameters for that class which you can specify from the command line as `smartdiffs show,separators,triggerSize,shortenSize,diffRatio,full`:


For the diffs arguments the values you can specify are:

  * `show` will not show anything (default is true)
  * `separators` allows to change the separators used to show the differences (default is "[]")
  * `triggerSize` controls the size above which the differences must be shown (default is 20)
  * `shortenSize` controls the number of characters to display around each difference (default is 5)
  * `diffRatio` percentage of differences above which the differences must not be shown (default is 30)
  * `full` displays the full original expected and actual strings

You can also specify your own enhanced algorithm for displaying the difference by providing an instance of the `${fullName[Diffs]}` trait:
```
trait Diffs {
  /** @return true if the differences must be shown */
  def show: Boolean

  /** @return true if the differences must be shown for 2 different strings */
  def show(expected: String, actual: String): Boolean

  /** @return the diffs */
  def showDiffs(expected: String, actual: String): (String, String)

  /** @return true if the full strings must also be shown */
  def showFull: Boolean
}
```

In this case pass the `diffsclass` argument with the class name.

### Colors

By default, the reporting will output colors. If you're running on windows you might either:

 * use the [following tip](http://www.marioawad.com/2010/11/16/ansi-command-line-colors-under-windows) to install colors in the DOS console
 * or pass `nocolor` as a command line argument

Then, there are different ways to set-up the colors you want to use for the output

*From system properties*

The so-called "SmartColors" argument will check if there are colors defined as specs2 properties. If so, the colors  used
to output text in the Console will be extracted from those properties:

e.g. `-Dspecs2.color.failure=m` will use magenta for failures.

The property names and default values are:

Property        | Default value |
--------------- |  ------------ |
`color.text`    |  white        |
`color.success` |  green        |
`color.failure` |  yellow       |
`color.error`   |  red          |
`color.pending` |  cyan         |
`color.skipped` |  magenta      |
`color.stats`   |  cyan         |

The default values above are provided for a black background. If you have a white background you can use the `specs2.whitebg` property and then the default values will be:

Property        | Default value |
--------------- |  ------------ |
`color.text`    |  black        |
`color.success` |  green        |
`color.failure` |  magenta      |
`color.error`   |  red          |
`color.pending` |  blue         |
`color.skipped` |  cyan         |
`color.stats`   |  blue         |

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


*From command-line arguments*

It is also possible to set colors by passing the `colors` argument. This argument must be a list of `key:value` pairs (comma-separated) where keys are taken from the property names above without the `color.` prefix and values from the abbreviated color names.

For example you can pass on the command line:

 `colors text:blue,failure:magenta`

 to have the text colored in blue and the failures in Magenta.

If the `colors` option contains `whitebg` then the default colors are considered to be [`InvertedColors`](${API_DIR}index.html#org.specs2.text.Colors)

*Through the API*

Finally you can change the color scheme that's being used on the console by implementing your own [`org.specs2.text.Colors`](${API_DIR}index.html#org.specs2.text.Colors) trait or override values in the existing `ConsoleColors` class. For example if you want to output magenta everywhere yellow is used you can write:

    object MyColors = new org.specs2.text.ConsoleColors { override val failureColor = magenta }

    class MyColoredSpecification extends Specification { def is = colors(MyColors) ^
      // the failure message will be magenta
      "this is a failing example" ! failure
    }

Note also that the the color support for sbt on Windows is a bit tricky. You need to follow the instructions [here](http://www.marioawad.com/2010/11/16/ansi-command-line-colors-under-windows) then add to your script launching sbt:

```
-Djline.terminal=jline.UnsupportedTerminal
```



"""
}

