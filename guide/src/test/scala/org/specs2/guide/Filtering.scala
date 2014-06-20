package org.specs2
package guide

object Filtering extends UserGuidePage { def is = s2"""
### Select an example to execute

Many specifications are written incrementally. You specify a little bit then you implement the application. When you go through this Specify-Implement-Execute cycle it is useful to be able to focus on just one example, the one you are currently working on. The `ex` (for "example") argument is what you need:
```
sbt> test-only *MySpecification* -- ex contains
```

The command above will execute any example which description matches the regular expression `.*contains.*` (which means that you can pass regular expressions in general). If you want to match on a few words you will need to delimit those words with `-ex` and `--` if there are other arguments:
```
sbt> test-only *MySpecification* -- -ex contains hello -- sequential
```

### Use tags

### Select failed examples

  *`ex`        *      | .*                                       | regular expression specifying the examples to execute. Use `ex .*brilliant.*` on the command line
 *`include`   *      | ""                                       | execute only the fragments tagged with any of the comma-separated list of tags: "t1,t2,..."
 *`exclude`   *      | ""                                       | do not execute the fragments tagged with any of the comma-separated list of tags: "t1,t2,..."
 *`wasIssue`  *      | false                                    | select only previously failed/error examples
 *`was`       *      | ""                                       | select only some previously executed examples based on their status

"""
}
