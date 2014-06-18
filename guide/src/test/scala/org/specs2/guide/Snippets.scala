package org.specs2
package guide

import specification.Snippets

object Snippets extends UserGuidePage { def is = s2"""
### Capture snippets

It is possible to include pieces of code in your documentation with the `${fullName[Snippets]}` trait using the `snippet` method to capture a block code with marker comments to delimit the parts you want to show.

What does this look like?

#### Snippet

Here is an example of using the `snippet` method:

```
s2$triple
This is a multi-line string with a snippet of code: $${ snippet {
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
factorial(3) == 6
}}
$triple
```

When you use the `snippet` method, the reports will show:

This is a multi-line string with a snippet of code:

```
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
factorial(3) == 6
```

#### Cuts

Since snippets are compiled code, you have to include many declarations, like imports or variables definitions, so that the code compiles. However this will make your documentation more verbose than necessary, so it is useful to hide them. One way to do this is to delimit the code to show with some comments of the form `// 8<--`:

```
s2$triple
This is a snippet of code with one relevant line: $${ snippet {
// 8<--
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
// 8<--
factorial(3) == 6
// 8<--
}}
$triple
```

The snippet above will only show `factorial(3) == 6`. You can actually repeat this pattern several times:

```
s2$triple
This is a snippet of code with 2 relevant lines: $${ snippet {
// 8<--
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
// 8<--
factorial(3) == 6
// 8<--
val n = 4
// 8<--
factorial(n) == 24
}}
$triple
```

This just displays:

```
factorial(3) == 6
factorial(n) == 24
```

#### Evaluation

By default the last value of a Snippet is not shown but you can display it with the `eval` method:

```
s2$triple
This is a snippet of code with a result: $${ snippet {
factorial(3)
}.eval}
$triple
```

This displays:

```
factorial(3)
```
```
> 6
```

#### Offsets

It is possible to adjust the margin of captured source code by adding or removing whitespace:

```
s2$triple
This is a snippet of code with a negative offset to align the code to the border of the screen: $${ snippet {
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
factorial(3)
}.offsetIs(-3)}
$triple
```

This displays:

```
factorial(3)
```

#### Parameters

All of the settings above: cuts, offset,... are coming from an implicit `SnippetParams` object that is changing the behavior of the created Snippets. You can choose, for a given scope, to replace these parameters with other ones and simply shadow the default parameters with your own, for example to always evaluate the snippets results:

     implicit snippetParams = SnippetParams(eval = true)

The parameters you can set are:

name              | description
----------------- | ----------------
 `trimExpression` | function that is trimming the expression from newlines or accolades
 `cutter`         | function to remove parts which must not be shown
 `asCode`         | function to render the resulting text (as Markdown for example)
 `prompt`         | function to display the evaluated result with a prompt
 `eval`           | boolean indicating if a snippet must be evaluated
 `verify`         | function checking the snippet value
"""
}
