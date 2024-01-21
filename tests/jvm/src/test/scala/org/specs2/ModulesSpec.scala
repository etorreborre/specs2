package org.specs2

import org.specs2.specification.core.*
import org.specs2.specification.create.*
import runner.SpecificationsFinder
import io.*

/** This is an example of running specifications module by module
  */
class ModulesSpec(env: Env) extends Specification {
  def is =
    br ^
      Fragments.foreach(specs)(s => link(showOnly("x!") ^ s) ^ br)

    def specs =
      List(Core(env), JUnit(env), Examples(env))
}

class Core(env: Env) extends Specification { def is = Module.specifications(env, getClass) }
class JUnit(env: Env) extends Specification { def is = Module.specifications(env, getClass) }
class Examples(env: Env) extends Specification { def is = Module.specifications(env, getClass) }

object Module extends SpecificationCreation:
  def specifications(env: Env, klass: Class[?], filter: String => Boolean = (s: String) => true) =
    val name = klass.getSimpleName.replace("$", "")
    val base = DirectoryPath.unsafe(new java.io.File(".").getAbsolutePath) / FileName.unsafe(
      name.toLowerCase
    ) / "src" / "test" / "scala"

    val finder = SpecificationsFinder.create(env)
    val specs =
      finder
        .findSpecifications(basePath = base, verbose = false, filter = filter)
        .unsafeRun
        .take(3)

    name.title.copy(specClass = klass) ^
      br ^
      Fragments.foreach(specs)(s => link(showOnly("!x") ^ s.is) ^ br)
