package org.specs2
package analysis

/**
 * This trait allows to define expected dependencies between packages layers
 */
trait LayersAnalysis extends DependencyFinder {

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
  case class Layer(names: Set[String], prefix: String = "", sourceDir: String = "src/main/scala/") {
    /** specify a prefix for this layer packages */
    def withPrefix(p: String) = copy(prefix = if (this.prefix.isEmpty) p else p+"."+this.prefix)
    /** specify a source directory for this layer packages */
    def inSourceDir(s: String) = copy(sourceDir = s)

    override def toString = names.mkString("\n") + (if (prefix == "") "" else " ("+prefix+")")
    /** @return the package names */
    lazy val packageNames = names.map(n => if (prefix.isEmpty) n else prefix+"."+n)

    /** @return the list of dependents of each package of this layer */
    lazy val getDependents: Set[Dependency] = packageNames.flatMap(p => getPackageDependents(p, sourceDir))

    /**
     * @return the list of dependencies showing that this layer depends on the `other` layer
     * meaning thisLayer -- depends on --> otherLayer
     */
    def dependsOn(otherLayer: Layer) = otherLayer.getDependents.filter(inThisLayer)

    /** @return true if the dependent class of this dependency has its package in this layer */
    private def inThisLayer(d: Dependency) = packageNames contains d.dependentPackageName

  }

  /**
   * This class represents an ordered set of layers, where each package on a given layer can only depend on packages defined in lower layers
   */
  case class Layers(layers: Seq[Layer]) {
    /** specify a prefix for all layers */
    def withPrefix(p: String) = copy(layers = layers.map(_.withPrefix(p)))
    /** specify a source directory for all layers */
    def inSourceDir(s: String) = copy(layers = layers.map(_.inSourceDir(s)))

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