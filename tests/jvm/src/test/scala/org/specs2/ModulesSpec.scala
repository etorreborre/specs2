package org.specs2

import org.specs2.specification.core.*
import org.specs2.specification.create.*
import runner.SpecificationsFinder
import io.*

/** This is an example of running specifications module by module
  */
class ModulesSpec extends Specification {
  def is =
    br ^
      Fragments.foreach(specs)(s => link(showOnly("x!") ^ s) ^ br)

    def specs =
      List(Core, JUnit, Examples)
}

object Core extends Specification { def is = Module.specifications(getClass) }
object JUnit extends Specification { def is = Module.specifications(getClass) }
object Examples extends Specification { def is = Module.specifications(getClass) }

object Module extends SpecificationCreation:
  def specifications(klass: Class[?], filter: String => Boolean = (s: String) => true) =
    val name = klass.getSimpleName.replace("$", "")
    val base = DirectoryPath.unsafe(new java.io.File(".").getAbsolutePath) / FileName.unsafe(
      name.toLowerCase
    ) / "src" / "test" / "scala"

    val env = EnvDefault.default
    val finder = SpecificationsFinder.create(env)
    val specs =
      finder.findSpecifications(basePath = base, verbose = false, filter = filter)
      .unsafeRun
      .take(3)

    env.shutdown()

    name.title.copy(specClass = klass) ^
      br ^
      Fragments.foreach(specs)(s => link(showOnly("!x") ^ s.is) ^ br)
