package org.specs2
package analysis

import matcher._

import org.scalacheck._
import Prop._
import collection.SeqGenerators._
import collection.Iterablex._

class LayersAnalysisSpec extends Specification with DependencyMatchers with ScalaCheck { def is =

                                                                                                                                            """
  It is possible to specify dependencies between packages as 'layers'. A `Layers` object is an ordered sequence of other `Layer`s
  A `Layer` is simply a list of package names, with possibly a prefix to factor out their common package
  by default all the packages are supposed to have corresponding class files in the src/main/scala directory, but it is possible to
  specify another directory.                                                                                                                """^
                                                                                                                                            p^
  "If all dependencies are respected between any 2 packages of different layers, there must be no unsatisfied dependencies"                 ! d1^
  "otherwise, it must display the failing dependencies"                                                                                     ! d2^
                                                                                                                                            end


  def d1 = forAll(okLayers) { (ls: Layers) => ls must beRespected }
  def d2 = forAll(koLayers) { (ls: Layers) => ls must not beRespected }.set(maxDiscarded -> 1000)

  /**
   * The data generation strategy is to create layers of packages represented by alphabetical letters from a to f.
   *
   * The ok layers are random but sorted so that the letter in one layer always precede the letters in a lower layer.
   * The ko layers must be unsorted
   *
   * Then the DependencyFinder trait is overriden so that it always provides an evidence that a package represented by a 'lower' letter depends
   * on a package represented by a 'higher' letter ('a' depends on 'c' for example)
   */
  // this generator makes sure that layers are sorted according to the alphabetical order
  lazy val okLayers = for {
    ls <- someSlicesOf("a", "b", "c", "d", "e", "f")
  } yield Layers(ls.map(l => Layer(l.toSet)).sortBy(_.names.mkString))

  // this generator makes sure that layers are unsorted according to the alphabetical order
  lazy val koLayers = for {
    ls <- okLayers
    ls2 = ls.copy(layers=ls.layers.scramble)
    if ls2.layers.sorted != ls2.layers
  } yield ls2

  // this generates a random Layer object from some package names
  lazy val layerGen: Gen[Layer] = Gen.someOf(packageNames).map(packages => Layer(packages.toSet))

  lazy val packageNames = Seq("a", "b", "c", "d", "e", "f")

  // return dependent packages according to the alphabetical order
  override def getPackageDependents(packageName: String, sourceDir: String) =
    dependentPackages(packageName).map(p => Dependency(packageName+".AClass", p+".ADependentClass"))

  // dependent packages are all the packages which are lower is the alphabetical order
  def dependentPackages(packageName: String) = packageNames.slice(0, packageNames.indexOf(packageName))

  implicit def seqStringOrdering: Ordering[Seq[String]]= new Ordering[Seq[String]] {
    def compare(x: Seq[String], y: Seq[String]): Int = implicitly[Ordering[String]].compare(x.mkString, y.mkString)
  }
  implicit def layerOrdering: Ordering[Layer]= new Ordering[Layer] {
    def compare(x: Layer, y: Layer): Int = implicitly[Ordering[Seq[String]]].compare(x.names.toSeq, y.names.toSeq)
  }

}