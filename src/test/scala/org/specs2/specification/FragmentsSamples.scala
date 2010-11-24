package org.specs2
package specification
import execute._

/**
 * This trait provides fragments to use for specifying folds and reporters
 */
trait FragmentsSamples extends FragmentsBuilder with StandardResults with PredefinedFragments {
  val text = Text("text")
  val ex1 = "ex1" ! success 
  val ex2 = "ex2" ! success
  val ex3 = "ex3" ! success
  val ex1Failure = "ex1" ! failure
  val ex1Error = "ex1" ! anError
  val ex1Skipped  = "ex1" ! skipped 
  val ex1Pending  = "ex1" ! pending 
  val exampleWithExpectations = "ex1" ! Success("ok", 2)

  val level1 = "level1" ^ ex1 ^ ex2
  val level2 = "level2" ^ ex1 ^ ex2
  val level2WithFailure = "level2" ^ ex1Failure ^ ex2
  val level1Level2 = level1 ^ end ^ level2 
  val level1ParLevel2 = level1 ^ p ^ level2 
  
}
