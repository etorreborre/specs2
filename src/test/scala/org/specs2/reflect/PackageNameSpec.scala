package org.specs2
package reflect

import PackageName._

class PackageNameSpec extends Specification { def is =

  { "org.specs2.runner".toPath === "org/specs2/runner/" } ^
                                                          end

}