package org.specs2
package analysis

import data.IncludedExcluded
import util.Properties._
/**
 * This trait allows to define expected dependencies between packages layers
 */
trait LayersAnalysis extends ClassycleDependencyFinder {

  /** this implicit definition allows to use a single string instead of a Layer object */
  implicit def toLayer(s: String): Layer = layer(s.split("\\s").map(l => l.trim):_*)
  /**
   * @return a new Layer object. If a prefix is defined, it will apply to each given name
   */
  def layer(names: String*): Layer = new Layer(names.filterNot(_.isEmpty).toSet)
  /**
   * @return a new Layers object, composed of other layers. If a prefix is defined it will apply to each layer
   */
  def layers(ls: Layer*) = Layers(ls)

  /**
   * The Layer class represent a list of packages.
   *
   * If those packages share a common prefix, it will be stored in the `prefix` member
   */
  case class Layer(names: Set[String], prefix: String = "",
                   sourceDir: String = "src/main/scala/",
                   targetDir: String = "target/scala-"+releaseVersion.orElse(developmentVersion).getOrElse("2.9.1")+"/classes/",
                   included: Seq[String] = Seq(), excluded: Seq[String] = Seq()) {
    /** specify a prefix for this layer packages */
    def withPrefix(p: String) = copy(prefix = if (this.prefix.isEmpty) p else p+"."+this.prefix)
    /** specify a source directory for this layer packages */
    def inSourceDir(s: String) = copy(sourceDir = s)
    /** specify a target directory for this layer packages */
    def inTargetDir(s: String) = copy(targetDir = s)

    override def toString = names.mkString("\n") + (if (prefix == "") "" else " ("+prefix+")")
    /** @return the package names */
    lazy val packageNames = names.map(n => if (prefix.isEmpty) n else prefix+"."+n)

    /** @return the list of dependents of each package of this layer */
    lazy val getDependents: Set[Dependency] = packageNames.flatMap(p => getPackageDependents(p, sourceDir, targetDir))

    /**
     * @return the list of dependencies showing that this layer depends on the `other` layer
     * meaning thisLayer -- depends on --> otherLayer
     */
    def dependsOn(otherLayer: Layer) = otherLayer.getDependents.filter(inThisLayer)

    /** use regexes to include fully qualified class names in the layer */
    def include(names: String*) = copy(included = names.map(n => ".*"+n+".*"))
    /** use regexes to exclude fully qualified class names from the layer */
    def exclude(names: String*) = copy(excluded = names.map(n => ".*"+n+".*"))

    /** @return true if the dependent class of this dependency has its package in this layer */
    def inThisLayer(d: Dependency) = includedExcluded.keep(d.dependentClassName) && (packageNames contains d.dependentPackageName)

    /**
     * check if a class name should be included or excluded from the dependency analysis, based on include/exclude regular expressions
     */
    private def includedExcluded = new IncludedExcluded[String] {
      val include = included
      val exclude = excluded

      val keepFunction    = (n: String, tags: Seq[String]) => tags.exists(t => n.matches(prefixed(t)))
    }

    private def prefixed(n: String) = if (prefix.isEmpty) n else prefix+"."+n
  }

  /**
   * This class represents an ordered set of layers, where each package on a given layer can only depend on packages defined in lower layers
   */
  case class Layers(layers: Seq[Layer]) {
    /** specify a prefix for all layers */
    def withPrefix(p: String) = copy(layers = layers.map(_.withPrefix(p)))
    /** specify a source directory for all layers */
    def inSourceDir(s: String) = copy(layers = layers.map(_.inSourceDir(s)))
    /** specify a target directory for all layers */
    def inTargetDir(s: String) = copy(layers = layers.map(_.inTargetDir(s)))

    override def toString = layers.mkString("\n")
    /** @return the layers as Markdown bullet points */
    lazy val toMarkdown: String = layers.map(l => " * "+l.names.mkString(" ")).mkString("\n")
    /** the list of dependencies which are not respecting the layers definitions */
    lazy val unsatisfied: Dependencies = new Dependencies(unsatisfiedDependencies)

    /**
     * @return the list of unsatisfied dependencies by:
     *
     * - taking all the subsequences of layers with at least 2 elements, say `Layers(l1, l2, l3) => Seq(Seq(l1, l2, l3), Seq(l1, l2))`
     * - getting all the dependencies of the last element of a sequence with all its parents (should always be empty)
     */
    private lazy val unsatisfiedDependencies = {
      layers.inits.filter(_.size > 1).flatMap(parents => parents.dropRight(1).flatMap(p => parents.last.dependsOn(p))).toSeq
    }
  }

}