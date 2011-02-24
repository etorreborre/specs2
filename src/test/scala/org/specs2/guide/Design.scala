package org.specs2
package guide

class Design extends Specification { def is = freetext                                                                  ^
  "specs2 design".title ^
                                                                                                                        """
<toc/>
                                                                                                                        
### Presentation

This page explains the design of _specs2_:

 * the structure of a specification
 * how the specification is built
 * how the specification is executed
 * how the reporting works

### Specification structure

The structure of a specification is very simple, it is just a list of `Fragments` provided by the `is` method of the
`SpecificationStructure` trait:

      +---------------+         1..n   +-----------+
      | Specification | -------------> | Fragment  |
      +---------------+                +-----------+
                                             ^
                                             |
             +----------+-----------+-----------+-------------+-------------+----------+                                          
             |          |           |           |             |             |          |
         +------+  +---------+  +-------+  +---------+  +-----------+  +---------+  +------+
         | Text |  | Example |  | Step  |  | Action  |  | SpecStart |  | SpecEnd |  | See  |
         +------+  +---------+  +-------+  +---------+  +-----------+  +---------+  +------+

Here's a short description of all Fragments:

 * Text: free text describing the specified system
 * Example: a description and a piece of executable code returning a Result
 * Step / Action: some action on the system which is only reported if there's an exception
 * SpecStart / SpecEnd: delimiters for the Specification. They also delimitate included Specifications. The SpecStart
   element also holds the Arguments used to tune the execution/reporting
 * See: a link to another specification

### Creating a Specification

#### Creating Fragments

There are implicits to create Fragments:

 * `String => Text`, to create a simple Text Fragment
 * `String ! Result => Example`, to create an Example
 * ...

Then, these Fragments can be "linked" with `^`, creating a `Fragments` object, containing a `Seq[Fragment]`:

      val fragments: Fragments =
        "this text" ^
        "is related to this Example" ! success

The `Fragments` object is used to hold temporarily that sequence of Fragments but it also makes sure that when the building
is done, the Fragments passed for execution will start and end with proper SpecStart and SpecEnd fragments.

#### In a mutable Specification

In a mutable Specification there is no visible "link" between Fragments, they're all created and linked through side-effects:

      // builds an Example and add it to the specFragments variable
      "this example must succeed" in { success }
      "same thing here" in { success }

### Specification execution

The execution is triggered by the various reporters and goes through 3 steps:

      // code from the Reporter trait
      spec.content |> select |> sequence |> execute

 1. Selection: the Fragments are filtered according to the Arguments object (for example with the `only("this example")` option)
 2. Sequence: the Fragments are sorted in groups so that all the elements of a group can be executed concurrently
 3. Execution: for each group the execution of the fragments is concurrent by default and results are collected in
    a sequence of `ExecutedFragments`

                                                                                                                        """^
                                                                                                                        end
}