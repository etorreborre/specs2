package org.specs2
package analysis

import specification.Analysis

class LayersSpec extends mutable.Specification with Analysis {

  "A layer contains package names which needs to be checked for dependencies" >> {
    "A layer can prefix its packages" >> {
      "p1 p2".withPrefix("com").packageNames must_== Set("com.p1", "com.p2")
    }

    "If another prefix is added, it goes before the first one" >> {
      "p1 p2".withPrefix("com").withPrefix("me").packageNames must_== Set("me.com.p1", "me.com.p2")
    }
  }

  "A dependency encapsulate the relationship between 2 dependent classes" >> {
    "A dependency can show a relationship between a class and its dependency" >> {
      Dependency("p1.classA", "p2.classDependingOnClassA").show must_== "p2 -> p1 (p2.classDependingOnClassA -> p1.classA)"
    }

    "A dependency can show a break between a class and its dependency" >> {
      Dependency("p1.classA", "p2.classDependingOnClassA").showBreak must_== "p1 x-> p2 because p2.classDependingOnClassA -> p1.classA"
    }
  }

  "Dependencies can be checked" >> {
    val layer1     = layer("com.p1", "com.p2")
    val dependency = Dependency("com.p3.SourceClass", "com.p1.TargetClass")

    "if the package of a dependent class is in a layer, then 'inThisLayer' is true" >> {
      layer1.inThisLayer(dependency) === true
    }
    "A class can be excluded from the dependency check" >> {
      layer1.exclude("TargetClass").inThisLayer(dependency) === false
    }

  }
}

class LayersWithCompilerSpec extends LayersSpec with CompilerDependencyFinder
