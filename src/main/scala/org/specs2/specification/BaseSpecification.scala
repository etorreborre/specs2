package org.specs2
package specification
import org.specs2.internal.scalaz._
import Scalaz._
import main.Arguments
import Fragments._
/**
 * A Base specification contains the minimum elements for a Specification
 * 
 * - a Seq of Fragments, available through the SpecificationStructure trait
 * - methods for creating Fragments from the FragmentsBuilder trait
 * - methods to include other specifications
 *
 */
trait BaseSpecification extends SpecificationStructure with FragmentsBuilder with SpecificationInclusion

/**
 * additional methods to include other specifications or Fragments
 */
trait SpecificationInclusion { this: FragmentsBuilder =>
  def include(f: Fragments): FragmentsFragment = fragmentsFragments(f)
  implicit def include(s: SpecificationStructure): FragmentsFragment = include(s.content)
  def include(s: SpecificationStructure, ss: SpecificationStructure*): FragmentsFragment = include(ma((Seq(s)++ss).map(_.content)).sum)
  def include(args: Arguments, s: SpecificationStructure*): FragmentsFragment = include(ma(s.map(_.content)).sum.overrideArgs(args))
}
/**
 * The structure of a Specification is simply defined as a sequence of fragments
 */
trait SpecificationStructure { 
  /** declaration of Fragments from the user */
  def is: Fragments
  /** this method can be overriden to map additional behavior in the user-defined fragments */
  def map(fs: =>Fragments): Fragments = fs
  /** 
   * this "cached" version of the Fragments is kept hidden from the user to avoid polluting
   * the Specification namespace.
   * SpecStart and SpecEnd fragments are added if the user haven't inserted any
   */
  private[specs2] lazy val content: Fragments = Fragments.withSpecName(map(is), this)
}

/**
 * methods for creating SpecificationStructure instances from fragments
 */
private[specs2]
object SpecificationStructure {
  import collection.Iterablex._
  
  def apply(fs: Fragments): SpecificationStructure = new SpecificationStructure {
    def is = fs.fragments match {
      case SpecStart(n,a,l,so) +: middle :+ SpecEnd(_) => Fragments(Some(n), middle, a, l, so)
      case other                                       => fs
    }
  }
  def apply(fs: Seq[Fragment]): SpecificationStructure = apply(Fragments.create(fs:_*))
}
