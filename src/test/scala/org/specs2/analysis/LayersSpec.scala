package org.specs2
package analysis

import specification._

class LayersSpec extends mutable.Specification with Analysis {

  "A layer can prefix its packages" >> {
    "p1 p2".withPrefix("com").packageNames must_== Set("com.p1", "com.p2")
  }

  "If another prefix is added, it goes before the first one" >> {
    "p1 p2".withPrefix("com").withPrefix("me").packageNames must_== Set("me.com.p1", "me.com.p2")
  }

  "A dependency can show a relationship between a class and its dependency" >> {
    Dependency("p1.classA", "p2.classDependingOnClassA").show must_== "p2 -> p1 (p2.classDependingOnClassA -> p1.classA)"
  }

  "A dependency can show a break between a class and its dependency" >> {
    Dependency("p1.classA", "p2.classDependingOnClassA").showBreak must_== "p1 x-> p2 because p2.classDependingOnClassA -> p1.classA"
  }

  "A class can be excluded from the dependency check" >> {
    val l = "p1 p2".withPrefix("com")
    l.inThisLayer(Dependency("p3.A", "com.p1.SpecificationWithJUnit")) === true
    l.exclude("SpecificationWithJUnit").inThisLayer(Dependency("p3.A", "com.p1.SpecificationWithJUnit")) === false
  }
}