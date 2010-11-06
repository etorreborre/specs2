package org.specs2
package specification

import control.Fold
/**
 * A FragmentFold transforms a list of fragments to a given type T, starting from an initial value.
 * It is either used to:
 * * map each fragment to something else 
 * * to accumulate values of type T
 * 
 * Different folds can be defined to process and report a specification. 
 * For example:
 * 
 * * a NestedLevels fold returns the "level" corresponding to a given Fragment (@see NestedLevels for more details)
 * * a FragmentsTree fold will take a list of fragments and return a tree of fragments according to
 *   their levels computed by the NestedLevels fold, optionally each Fragment node can be mapped to another object
 * * a JUnitDescription fold takes a list of fragments and returns a tree of Description objects corresponding to
 *   each Fragment  
 * 
 */
trait FragmentFold extends Fold[Fragment]
trait ExecutedFragmentFold extends Fold[ExecutedFragment]





