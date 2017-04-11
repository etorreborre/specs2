package org.specs2
package specification
package core

import main.Arguments
import org.specs2.control._
import org.specs2.data.TopologicalSort
import control._
import org.specs2.fp.syntax._
import org.specs2.fp.syntax._
import process._

/**
 * Structure of a Specification:
 *
 *  - a header
 *  - some arguments
 *  - specification fragments
 *
 * Note that the fragments have to be lazy in order to avoid cycles when 2 specifications are referencing
 * each other with links
 */
case class SpecStructure(header: SpecHeader, arguments: Arguments, lazyFragments: () => Fragments) {
  lazy val fragments = lazyFragments()

  def contents: AsyncStream[Fragment]                               = fragments.contents
  def map(f: Fragments => Fragments): SpecStructure                 = copy(lazyFragments = () => f(fragments))
  def |>(p: AsyncTransducer[Fragment, Fragment]): SpecStructure     = copy(lazyFragments = () => fragments |> p)
  def update(f: AsyncTransducer[Fragment, Fragment]): SpecStructure = copy(lazyFragments = () => fragments update f)
  def flatMap(f: Fragment => AsyncStream[Fragment]): SpecStructure  = |>(_.flatMap(f))

  def setHeader(h: SpecHeader) = copy(header = h)
  def setArguments(args: Arguments) = copy(arguments = args)
  def setFragments(fs: =>Fragments) = copy(lazyFragments = () => fs)

  def specClassName = header.className
  def name = header.title.getOrElse(header.simpleName)
  def wordsTitle = header.title.getOrElse(header.wordsTitle)

  def texts = fragments.texts
  def examples = fragments.examples
  def tags = fragments.tags

  def references = fragments.referenced
  def specificationRefs = fragments.specificationRefs

  def seeReferences = fragments.seeReferences

  def linkReferences = fragments.linkReferences

  def dependsOn(spec2: SpecStructure): Boolean =
    SpecStructure.dependsOn(this, spec2)
}

/**
 * Create SpecStructures from header, arguments, fragments
 */
object SpecStructure {
  def apply(header: SpecHeader): SpecStructure =
    new SpecStructure(header, Arguments(), () => Fragments())

  def apply(header: SpecHeader, arguments: Arguments): SpecStructure =
    new SpecStructure(header, arguments, () => Fragments())

  def create(header: SpecHeader, fragments: =>Fragments): SpecStructure =
    new SpecStructure(header, Arguments(), () => fragments)

  def create(header: SpecHeader, arguments: Arguments, fragments: =>Fragments): SpecStructure =
    new SpecStructure(header, arguments, () => fragments)

  /**
   * sort the specifications in topological order where specification i doesn't depend on specification j if i > j
   *
   * means "dependents first"!
   */
  def topologicalSort(specifications: Seq[SpecStructure]): Option[Vector[SpecStructure]] =
    TopologicalSort.sort(specifications, (s1: SpecStructure, s2: SpecStructure) => dependsOn(s2, s1))

  /**
   * sort the specifications in topological order where specification i doesn't depend on specification j if i > j
   *
   * means "dependents last"!
   */
  def reverseTopologicalSort(specifications: Seq[SpecStructure]): Option[Vector[SpecStructure]] =
    TopologicalSort.sort(specifications, dependsOn)

  /** return true if s1 depends on s2, i.e, s1 has a link to s2 */
  val dependsOn = (s1: SpecStructure, s2: SpecStructure) => {
    val s1Links = s1.fragments.fragments.collect(Fragment.linkReference).map(_.specClassName)
    s1Links.contains(s2.specClassName)
  }

  def empty(klass: Class[_]) =
    SpecStructure(SpecHeader(klass))

  /** @return all the referenced specifications */
  def referencedSpecStructures(spec: SpecStructure, env: Env, classLoader: ClassLoader): Operation[Seq[SpecStructure]] =
    specStructuresRefs(spec, env, classLoader)(referencedSpecStructuresRefs(env))

  /** @return all the linked specifications */
  def linkedSpecifications(spec: SpecStructure, env: Env, classLoader: ClassLoader): Operation[Seq[SpecStructure]] =
    specStructuresRefs(spec, env, classLoader)(linkedSpecStructuresRefs(env))

  /** @return all the see specifications */
  def seeSpecifications(spec: SpecStructure, env: Env, classLoader: ClassLoader): Operation[Seq[SpecStructure]] =
    specStructuresRefs(spec, env, classLoader)(seeSpecStructuresRefs(env))

  /** @return all the referenced spec structures */
  def specStructuresRefs(spec: SpecStructure, env: Env,
                         classLoader: ClassLoader)(refs: SpecStructure => List[SpecificationRef]): Operation[Seq[SpecStructure]] = {

    val byName = (ss: List[SpecStructure]) => ss.foldLeft(Vector[(String, SpecStructure)]()) { (res, cur) =>
      val name = cur.specClassName
      if (res.map(_._1).contains(name)) res
      (name, cur) +: res
    }

    def getRefs(s: SpecStructure, visited: Vector[(String, SpecStructure)]): Vector[(String, SpecStructure)] =
      refs(s).map { ref =>
        SpecificationStructure.create(ref.header.specClass.getName, classLoader, Some(env)).map(_.structure(env).setArguments(ref.arguments))
      }.sequence.map(byName).runOption.getOrElse(Vector())
       .filterNot { case (n, _) => visited.map(_._1).contains(n) }

    Operations.delayed {
      def getAll(seed: Vector[SpecStructure], visited: Vector[(String, SpecStructure)]): Vector[SpecStructure] = {
        if (seed.isEmpty) visited.map(_._2)
        else {
          val toVisit: Vector[(String, SpecStructure)] = seed.flatMap(s => getRefs(s, visited))
          getAll(toVisit.map(_._2), visited ++ toVisit)
        }
      }
      val name = spec.specClassName
      val linked = getRefs(spec, Vector((name, spec)))
      getAll(linked.map(_._2), linked :+ ((name, spec)))
    }
  }

  /** @return the class names of all the referenced specifications */
  def referencedSpecStructuresRefs(env: Env)(spec: SpecStructure): List[SpecificationRef] =
    select(env)(spec).fragments.fragments.collect(Fragment.specificationRef).toList

  /** @return the class names of all the linked specifications */
  def linkedSpecStructuresRefs(env: Env)(spec: SpecStructure): List[SpecificationRef] =
    select(env)(spec).fragments.fragments.collect(Fragment.linkReference).toList

  /** @return the class names of all the see specifications */
  def seeSpecStructuresRefs(env: Env)(spec: SpecStructure): List[SpecificationRef] =
    select(env)(spec).fragments.fragments.collect(Fragment.seeReference).toList

  /** @return select only the fragments according to the current arguments */
  def select(env: Env)(spec: SpecStructure): SpecStructure =
    spec.map(fs => fs |> DefaultSelector.select(env))

}
