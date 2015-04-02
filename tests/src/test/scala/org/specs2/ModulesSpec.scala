package org.specs2

import org.specs2.specification.core._
import org.specs2.specification.create._
import runner.SpecificationsFinder
import io._

/**
 * This is an example of running specifications module by module
 */
class ModulesSpec extends Specification { def is =
 br ^
 Fragments.foreach(specs)(s => link(s) ^ br)

 def specs =
   List(Core, JUnit, Examples)
}

object Core     extends Specification { def is = Module.specifications(getClass) }
object JUnit    extends Specification { def is = Module.specifications(getClass) }
object Examples extends Specification { def is = Module.specifications(getClass) }

object Module extends SpecificationCreation {
  def specifications(klass: Class[_], filter: String => Boolean = (s: String) => true) = {
    val name = klass.getSimpleName.replace("$", "")
    val base = DirectoryPath.unsafe(new java.io.File(".").getAbsolutePath) / FileName.unsafe(name.toLowerCase) / "src" / "test" / "scala"
    val specs = SpecificationsFinder.specifications(basePath = base, verbose = false, filter = filter).take(3)

    name.title.copy(specClass = klass) ^
    br ^
    Fragments.foreach(specs)(s => link(showOnly("!x") ^ s.is) ^ br)
  }
}
