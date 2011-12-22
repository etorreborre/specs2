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

}