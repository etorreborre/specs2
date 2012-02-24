package org.specs2
package reporter

import main.Arguments
import control.LazyParameters._
import specification._
import Fragments._
import SpecsArguments._
import execute.Result

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
          case e @ Example(_,_) if e.isolable => e.copy(body = () => copyBody(name, e.body(), index))
          case a @ Action(_) if a.isolable    => a.copy(action = lazyfy(copyBody(name, a.execute, index)))
          case other                          => other
        }
      } else fragment
    }
  }

  /**
   * @return an Example which body comes from the execution of that example in a brand new instance of the Specification
   */
  protected def copyBody(name: SpecName, body: =>Result, index: Int) = {
    SpecificationStructure.createSpecificationOption(name.javaClassName).map { specification =>
      val fragments = specification.is.fragments
      def executeStepsBefore(n: Int) = fragments.zipWithIndex.collect { case (s @ Step(_), i) if i < n && s.isolable => s.execute }
      fragments(index) match {
        case e @ Example(_, _) => executeStepsBefore(index); e.execute
        case a @ Action(_)     => executeStepsBefore(index); a.execute
        case other             => body
      }
    }.getOrElse(body)
  }

  protected def isolateSteps(fragments: Seq[Fragment])(implicit arguments: Arguments): Seq[FragmentSeq] = {
    fragments.foldLeft(Vector(): Seq[FragmentSeq]) { (res, f) =>
      res.toList match {
        case Nil => Vector(FragmentSeq.create(f))
        case last :: rest => f match {
          case SpecStart(_,_,_,_) | SpecEnd(_)                    => FragmentSeq.create(f) +: res
          case Step(_) if last.fragments.exists(isExampleOrStep)  => FragmentSeq.create(f) +: res
          case Example(_, _) if last.fragments.exists(isStep)     => FragmentSeq.create(f) +: res
          case any if last.fragments.exists(isSpecStartOrEnd)     => FragmentSeq.create(f) +: res
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
