package org.specs2
package reporter

import main.Arguments
import control.LazyParameters._
import specification._
import Fragments._
import SpecsArguments._

/**
 * The Sequence trait implements the logic for the fragments to execute according to their dependencies
 */
trait Sequence {
  /** select function returning a filtered and ordered seq of seq of Fragments */
  def sequence(implicit arguments: Arguments): SpecificationStructure => ExecutableSpecification
}

/**
 * this case class transports the fragments to execute, grouped in sequences of examples which can be executed concurrently
 */
case class ExecutableSpecification(name: SpecName, arguments: Arguments, fs: Seq[FragmentSeq]) {
  def map(f: Fragment => Fragment) = copy(fs = fs.map(seq => seq map f))
  def fragments = fs.flatMap(fseq => fseq.fragments)
}

/**
 * The DefaultSequence trait sorts the Fragments by making sure Steps will be executed before Examples
 */
trait DefaultSequence {
  /** sequence function returning an ordered seq of seq of Fragments */
  def sequence(implicit arguments: Arguments): SpecificationStructure => ExecutableSpecification = (spec: SpecificationStructure) =>
    ExecutableSpecification(spec.content.specName, spec.content.arguments, sequence(spec.content.specName, spec.content.fragments)(arguments))

  /**
   * the sequence method returns sequences of fragments which can be executed concurrently.
   * Among the constraints to respect, any Step Fragment must be executed before any following Example Fragment,
   * so that "first" and "last" actions are executed properly in a Specification
   *
   * If the arguments specify that the specification is sequential, then each fragment will be executed individually
   */
  def sequence(specName: SpecName, fragments: Seq[Fragment])(implicit arguments: Arguments = Arguments()): Seq[FragmentSeq] = {
    // isolate examples if necessary, using the arguments of the current specification in case of included specifications
    val fs = SpecsArguments.foldAll(fragments).filter(isolateExamples)
    if (arguments.sequential) fs.map(f => FragmentSeq.create(f))
    else                      isolateSteps(fs)(arguments).reverse
  }

  /**
   * This function "clones" the body of each example if the applicable arguments indicate that the specification should
   * be isolated
   */
  protected def isolateExamples(implicit arguments: Arguments) = (fs: Seq[(Fragment, Arguments, SpecName)])=> {
    fs.zipWithIndex.map { fani  =>
      val ((fragment, args, name), index) = fani
      if ((arguments <| args).isolated) {
        fragment match {
          case e @ Example(_,_) => e.copy(body = () => copyBody(name, e, index))
          case other            => other
        }
      } else fragment
    }
  }

  /**
   * @return an Example which body comes from the execution of that example in a brand new instance of the Specification
   */
  protected def copyBody(name: SpecName, e: Example, index: Int) = {
    SpecificationStructure.createSpecificationOption(name.javaClassName).map { specification =>
      specification.is.fragments(index) match {
        case Example(_, body) => body()
        case other            => e.body()
      }
    }.getOrElse(e.body())
  }




  protected def isolateSteps(fragments: Seq[Fragment])(implicit arguments: Arguments): Seq[FragmentSeq] = {
    fragments.foldLeft(Vector(): Seq[FragmentSeq]) { (res, f) =>
      res.toList match {
        case Nil => Vector(FragmentSeq.create(f))
        case last :: rest => f match {
          case Step(_) if last.fragments.exists(isExampleOrStep)  => FragmentSeq.create(f) +: last +: rest.toSeq
          case Example(_, _) if last.fragments.exists(isStep)     => FragmentSeq.create(f) +: last +: rest.toSeq
          case _                                                  => FragmentSeq(last.fragments :+ f) +: rest.toSeq
        }
      }
    }
  }
}
case class FragmentSeq(fragments: Seq[Fragment]) {
  def arguments = Fragments.create(fragments:_*).arguments

  def map(f: Fragment => Fragment) = copy(fragments = fragments map f)
}
case object FragmentSeq {
  def create(f: Fragment) = FragmentSeq(Seq(f))
}
