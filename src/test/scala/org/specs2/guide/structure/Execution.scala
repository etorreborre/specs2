package org.specs2
package guide
package structure

object Execution extends UserGuideVariables {

  def section = s"""
### Execution

This section summarizes the execution algorithm of a specification based on its fragments:

 1. all the fragments are divided into groups delimited by `Steps`
 2. if the `sequential` argument is present, each fragment goes to its own group
 3. groups are executed sequentially and all the fragments of a given group are executed concurrently
 4. if the `isolated` argument is present, each example is executed in its own version of the Specification
 5. if the `isolated` argument is present, all the `Steps` preceding an example are executed before that example
 6. if the Specification inherits from the `AllExpectations` trait, then it is executed as an `isolated` Specification unless it is already set as `sequential`
 7. if the `stopOnFail` argument is present, all the examples in the next group of fragments will be skipped if there is a failure in one of the previous groups
 8. if the `stopOnSkip` argument is present, all the examples in the next group of fragments will be skipped if there is a skipped in one of the previous groups
 9. if there is a `Step` created with `Step.stopOnFail` or `Step.stopOnFail(when = true)`, all the examples in the next group of fragments will be skipped if there is a failure in the group before the `Step`
  """

}
