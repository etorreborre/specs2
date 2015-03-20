package org.specs2
package guide

import org.specs2.data.AlwaysTag
import org.specs2.specification.core.Fragments

object Selection extends UserGuidePage { def is = s2"""

Many specifications are written incrementally. You specify a little bit then you implement the application. When you go through this "Specify-Implement-Execute" cycle it is useful to be able to focus on just one example, the one you are currently working on. The `ex` argument is what you need (`ex` stands for "example"):
```
sbt> test-only *MySpecification* -- ex contains
```

The command above will execute any example which description matches the regular expression `.*contains.*` (which means that you can pass regular expressions in general). If you want to match a few words you will need to use double quotes:
```
sbt> test-only *MySpecification* -- ex "contains hello" sequential
```

### Use tags

Tags can be used in a Specification to include or exclude some examples or a complete section of fragments from the execution. Let's have a look at one example: ${snippet{
class TaggedSpecification extends Specification { def is = s2"""
 this is some introductory text
  and the first group of examples
  example 1 $success                         ${tag("feature1", "unit")}
  example 2 $success                         ${tag("integration")}

  and the second group of examples           ${section("checkin")}
  example 3 $success
  example 4 $success                         ${section("checkin")}
                                             """
  }
}}

In that specification we are defining several tags and sections:

* `feature 1` is a tag that is applied to `example1` (the _preceding_ Fragment)
* `feature 2` is a tag that is applied to `example2` (the _preceding_ Fragment)
* `checkin` marks a section which goes from the Text `and the second group of examples` to `example 4`

Armed with this, it is now easy to include or exclude portions of the specification at execution time:

* `include feature1` will only include `example 1`
* `exclude integration` will include everything except `example 2`
* `include checkin,unit` will include anything having either `checkin` OR `unit`: i.e. `example 1` and the second group of examples (`example 3` and `example 4`)
* `include feature1 && unit` will include anything having `feature1` AND `unit`: i.e. `example 1`
* `include feature1 && unit, checkin` will include anything having `feature1` AND `unit`, OR having `checkin`: i.e. `example 1`, `example 3`, `example4`

#### In a unit specification

A _unit_ specification will accept the same `tag` and `section` methods but the behavior will be slightly different: ${snippet{

import org.specs2.mutable._

class TaggedSpecification extends Specification {
  "this is some introductory text" >> {
    "and the first group of examples" >> {
      tag("feature 1", "unit")
      "example 1" in success
      "example 2" in success
    }
  }
  section("checkin")
  "and the second group of examples" >> {
    "example 3" in success
    "example 4" in success
  }
  section("checkin")

  "and the last group of examples" >> {
    "example 5" in success
    "example 6" in success
  }
}
}}

For that specification above:

 * when the `tag` call is inserted on a new line, the tagged fragment is the one just _after_ the tag method call: `example 1`
 is tagged with `feature1 and unit`,

 * when the `tag` is appended to an example, it applies to that example: `example 2` is tagged with `integration`

 * when the `section` call is inserted on a new line, this opens a section for all the following fragments. This should
 be closed by a corresponding `section` call on a new line. For example, `example 3` and `example 4` are part of the
 "checkin" section

 * when the `section` call is appended to a block of Fragments on the same line, all the fragments of that block are part of
 the section: `example 5` and `example 6` are tagged with `slow`

#### `Always` tag

Some specifications need to have `Steps` which will always be included whatever tags are specified on the command line. This is the case when defining a ${""""template" specification""" ~/ SpecificationTemplate} with setup/teardown steps: ${snippet{
trait DatabaseSpec extends Specification {
  override def map(fs: =>Fragments) =
    step("startDb") ^ tag(AlwaysTag) ^
    fs ^
    step("cleanDb") ^ tag(AlwaysTag)
}
}}

### Select failed examples

Another frequent mode of selection is the selection based on previous execution. Generally we want to re-execute only what was broken before. For this, using the `was` argument on the command-line:
```
sbt> test-only *MyFailedSpecification* -- was x
```

On the line above `x` is the status of the previous example. Here is a table of all the flags you can use:

  Flag | Description
 ----- | ------------
  `+`  | successful example
  `x`  | failed example
  `!`  | error example
  `o`  | skipped example
  `*`  | pending example
  `-`  | text
  `1`  | statistics

This selection works because $specs2 stores the state of each specification in a directory after a run (`target/specs2-reports/stats` by default). If you decide that this storing is useless and you want to skip it you can use the `neverstore` argument. Otherwise if you want to make sure that the `stats` directory doesn't become too big over time you can use the `resetstore` argument which will remove the current store before running the specification.

$AndIfYouWantToKnowMore

 - learn how to ${"display only some examples" ~/ ConsoleOutput}
 - consult the ${"arguments reference guide" ~/ ArgumentsReference} for a list of all arguments

$vid


"""
}
