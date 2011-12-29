package org.specs2
package specification

import ExecutedFragment._


/**
 * A specification with a name and all of its fragments already executed
 */
case class ExecutedSpecification(name: SpecName, fs: Seq[ExecutedFragment]) {

  def includedLinkedSpecifications: Seq[ExecutedSpecStart]  = fragments collect isIncludeLink
  def includedSeeOnlySpecifications: Seq[ExecutedSpecStart] = fragments collect isSeeOnlyLink

  /** @return the executed fragments */
  def fragments = fs

}