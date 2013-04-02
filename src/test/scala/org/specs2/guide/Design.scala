package org.specs2
package guide

class Design extends UserGuidePage { def is =
  """
### Presentation

This page explains the overall design of _specs2_:

 * the structure of a specification
 * how the specification is built
 * how the specification is executed
 * how the reporting works
 * the packages dependencies

_note_: some details might not be completely up to date as the code base evolves.

### Structure

The structure of a specification is very simple, it is just a list of `Fragments` provided by the `is` method of the
`SpecificationStructure` trait:

    +---------------+                     1..n  +-----------+
    | Specification | ------------------------> | Fragment  |
    +---------------+                           +-----------+
                                                      ^
                                                      |
                  +----------+-----------+-----------+-------------+-------------+---------------+
                  |          |           |           |             |             |               |
              +------+  +---------+  +-------+  +---------+  +-----------+  +---------+  +-----------------+
              | Text |  | Example |  | Step  |  | Action  |  | SpecStart |  | SpecEnd |  | TaggingFragment |
              +------+  +---------+  +-------+  +---------+  +-----------+  +---------+  +-----------------+


Here's a short description of all the Fragments:

 * Text: free text describing the specified system
 * Example: a description and a piece of executable code returning a Result
 * Step / Action: some action on the system which is only reported if there's an exception
 * SpecStart / SpecEnd: delimiters for the Specification. They also delimitate included Specifications.
   The SpecStart element holds: the Arguments used to tune the execution/reporting, the link to an included/referenced specification
 * TaggingFragments: those fragments enclose other fragments which can be included or excluded from the execution

### Creation

#### Creating Fragments

There are implicits to create Fragments (found in the `org.specs2.specification.FragmentsBuilder` trait):

 * `String => Text`, to create a simple Text Fragment
 * `String ! Result => Example`, to create an Example
 * ...

Once built, these Fragments can be "linked" with `^`, creating a `Fragments` object, containing a `Seq[Fragment]`:

      val fragments: Fragments =
        "this text" ^
        "is related to this Example" ! success

The `Fragments` object is used to hold temporarily a sequence of Fragments as it is built and it makes sure that when the building is done, the Fragments passed for execution will start and end with proper SpecStart and SpecEnd fragments.

#### Mutable Specification

In a mutable Specification there is no visible "link" between Fragments, they're all created and linked through side-effects (thanks to an enhanced version of the `FragmentsBuilder` trait in the `org.specs2.mutable` package):

      // build an Example and add it to the specFragments variable
      "this example must succeed" in { success }
      "same thing here" in { success }

Of course this there is mutation involved here, it's not advised to do anything concurrent at that point.

### Execution

The execution is triggered by the various reporters and goes through 5 steps:

      // code from the Reporter trait
      spec.content |> select |> sequence |> execute |> store |> export(spec)

 1. <u>Selection</u>: the Fragments are filtered according to the Arguments object. In that phase all examples but a few can be filtered if the `only("this example")` option is used for instance. Another way to select fragments is to insert `TaggingFragment`s inside the specification.
    If the `isolated` argument is true, each example body is replaced with the same body executing in a cloned Specification to avoid seeing side-effects on local variables.

 2. <u>Sequencing</u>: the Fragments are sorted in groups so that all the elements of a group can be executed concurrently. This usually why Steps are used. If my fragments are: `fragments1 ^ step ^ fragments2` then all fragments1 will be executed,
    then step, then fragments2.

 3. <u>Execution</u>: for each group, the execution of the fragments is concurrent by default and results are collected in
    a sequence of `ExecutingFragments`. We don't wait for the execution of all the Fragments to be finished before starting the reporting.

 4. <u>Storing</u>: after an execution we compute the statistics for each specification and store the results in a file (`specs2-reports/specs2.stats`).
    This allows to do consequent runs based on previous executions: to execute failed specifications only or to create the index page with an indicator of previously executed specifications

 5. <u>Exporting</u>: depending on the exporter, the ExecutedFragments are translated to `PrintLines` or `HtmlLines` to be flushed out to the console or in an html file

### Reporting

All the reporters start with a sequence of `ExecutingFragment`s. A list of `Reducers` is used to collect relevant information:

 * The text and results to display
 * The "level" of the text i.e. its indentation. The rules for this are given in the Layout section of the [Specification Structure](org.specs2.guide.Structure.html#Rules) page
 * The statistics and execution times
 * The applicable arguments (where the arguments of an included specification must override the arguments of its parent)

One of the main difficulties in this 'reduction' is the fact that included specifications change the context of what needs to be accumulated. The `reporter.NestedBlocks` trait provides functions to handle this.

Then, each fragment and associated data (level, statistics, arguments,...) is translated to a display element:

 * for a console output, `PrintLines`: `PrintSpecStart`, `PrintText`, `PrintResult`,...
 * for a Html output, `HtmlLines`: `HtmlSpecStart`, `HtmlText`, `HtmlResult`,...
 * for a JUnit output, a tree of JUnit `Description` objects with the corresponding code to execute (in JUnit the Descriptions
    are built first, then the examples are executed)

### Dependencies

The following package dependencies should be always verified, from low-level packages to high-level ones, where no package on a low layer can depend on a package on a higher layer:

    + runner
    + reporter
    + mutable    specification
    + mock form
    + matcher
    + execute
    + analysis reflect  xml html  time json
    + collection control  io  text  main data



                                                                                                                        """
}