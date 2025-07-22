package org.specs2
package specification
package core

import data.TopologicalSort
import control.*
import reflect.Classes
import Classes.*
import org.specs2.fp.syntax.*

@org.portablescala.reflect.annotation.EnableReflectiveInstantiation
trait SpecificationStructure:
  def is: SpecStructure

  def structure: SpecStructure =
    decorate(is)

  def decorate(is: SpecStructure): SpecStructure =
    map(is.map(fs => map(fs.flatMap(f => flatMap(f).contents))))

  /** modify the specification structure */
  def map(structure: SpecStructure): SpecStructure =
    structure

  /** modify the fragments */
  def map(fs: =>Fragments): Fragments = fs

  /** modify the fragments */
  def flatMap(f: Fragment): Fragments =
    Fragments(f)

object SpecificationStructure:

  /** create a SpecificationStructure from a class name
    */
  def create(
      className: String,
      classLoader: ClassLoader = Thread.currentThread.getContextClassLoader,
      env: Option[Env] = None
  ): Operation[SpecificationStructure] =
    lazy val defaultInstances = env.toList.flatMap(_.defaultInstances)

    // make sure the instantiated class is a Specification Structure (see #477)
    def asSpecificationStructure(i: Any): Operation[SpecificationStructure] =
      Operation.delayed(classOf[SpecificationStructure].cast(i))

    existsClass(className + "$", classLoader) flatMap { e =>
      if e then
        // try to create the specification from the object name
        createInstance[SpecificationStructure](className + "$", classLoader, defaultInstances)
          .flatMap(asSpecificationStructure)
          .orElse(
            // fallback to the class if this is just a companion object
            createInstance[SpecificationStructure](className, classLoader, defaultInstances)
              .flatMap(asSpecificationStructure)
          )
      else
        // try to create the specification from a class name
        createInstance[SpecificationStructure](className, classLoader, defaultInstances).flatMap(
          asSpecificationStructure
        )
    }

  /** sort the specifications in topological order where specification i doesn't depend on specification j if i > j
    *
    * means "dependents first"!
    */
  def topologicalSort(env: Env) = (specifications: Seq[SpecificationStructure]) =>
    TopologicalSort.sort(
      specifications,
      (s1: SpecificationStructure, s2: SpecificationStructure) =>
        SpecStructure.dependsOn(env.specs2ExecutionEnv)(s2.structure, s1.structure)
    )

  /** sort the specifications in topological order where specification i doesn't depend on specification j if i < j
    *
    * means "dependents last"!
    */
  def reverseTopologicalSort(env: Env) = (specifications: Seq[SpecificationStructure]) =>
    TopologicalSort.sort(
      specifications,
      (s1: SpecificationStructure, s2: SpecificationStructure) =>
        SpecStructure.dependsOn(env.specs2ExecutionEnv)(s1.structure, s2.structure)
    )

  /** @return all the referenced specifications */
  def referencedSpecifications(
      spec: SpecificationStructure,
      env: Env,
      classLoader: ClassLoader
  ): Operation[Seq[SpecificationStructure]] =
    specificationsRefs(spec, env, classLoader)(referencedSpecificationsRefs)

  /** @return all the linked specifications */
  def linkedSpecifications(
      spec: SpecificationStructure,
      env: Env,
      classLoader: ClassLoader
  ): Operation[Seq[SpecificationStructure]] =
    specificationsRefs(spec, env, classLoader)(linkedSpecificationsRefs)

  /** @return all the see specifications */
  def seeSpecifications(
      spec: SpecificationStructure,
      env: Env,
      classLoader: ClassLoader
  ): Operation[Seq[SpecificationStructure]] =
    specificationsRefs(spec, env, classLoader)(seeSpecificationsRefs)

  /** @return all the referenced specifications */
  def specificationsRefs(spec: SpecificationStructure, env: Env, classLoader: ClassLoader)(
      refs: (SpecificationStructure, Env) => List[SpecificationRef]
  ): Operation[Seq[SpecificationStructure]] =

    val byName = (ss: List[SpecificationStructure]) =>
      ss.foldLeft(Vector[(String, SpecificationStructure)]()) { (res, cur) =>
        val name = cur.structure.specClassName
        if res.map(_._1).contains(name) then res
        else (name, cur) +: res
      }

    def getRefs(
        s: SpecificationStructure,
        visited: Vector[(String, SpecificationStructure)]
    ): Vector[(String, SpecificationStructure)] =
      refs(s, env)
        .map(ref => create(ref.header.specClass.getName, classLoader, Some(env)))
        .sequence
        .map(byName)
        .runMonoid
        .filterNot { case (n, _) => visited.map(_._1).contains(n) }

    Operation.delayed {
      def getAll(
          seed: Vector[SpecificationStructure],
          visited: Vector[(String, SpecificationStructure)]
      ): Vector[SpecificationStructure] =
        if seed.isEmpty then visited.map(_._2)
        else
          val toVisit: Vector[(String, SpecificationStructure)] = seed.flatMap(s => getRefs(s, visited))
          getAll(toVisit.map(_._2), visited ++ toVisit)
      val name = spec.structure.specClassName
      val linked = getRefs(spec, Vector((name, spec)))
      getAll(linked.map(_._2), linked :+ ((name, spec)))
    }

  /** @return the class names of all the referenced specifications */
  def referencedSpecificationsRefs(spec: SpecificationStructure, env: Env): List[SpecificationRef] =
    SpecStructure.referencedSpecStructuresRefs(env)(spec.structure)

  /** @return the class names of all the linked specifications */
  def linkedSpecificationsRefs(spec: SpecificationStructure, env: Env): List[SpecificationRef] =
    SpecStructure.linkedSpecStructuresRefs(env)(spec.structure)

  /** @return the class names of all the see specifications */
  def seeSpecificationsRefs(spec: SpecificationStructure, env: Env): List[SpecificationRef] =
    SpecStructure.seeSpecStructuresRefs(env)(spec.structure)
