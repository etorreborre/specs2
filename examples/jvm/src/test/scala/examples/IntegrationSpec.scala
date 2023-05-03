package examples

import org.specs2.*
import org.specs2.specification.core.*
import org.specs2.control.*
import org.specs2.io.*
import org.specs2.runner.*
import integration.*
import scala.concurrent.ExecutionContext

class IntegrationSpec(using ec: ExecutionContext) extends Specification, StartDatabase:
  def is = sequential ^ s2"""

  Integration specifications
  $integration"""

  def integration =
    Fragments.foreach(specifications) { specification =>
      s2"""
      $br${specification.is.header.name}${specification.is.fragments}"""

    }

  val specifications = SpecificationsFinder.default
    .findSpecifications(
      // change this pattern if the specifications must be found in specific directories, or with specific names
      glob = "**/*.scala",
      // this pattern detects the name of a specification class inside a scala file
      pattern = "(.*Spec).*\\s*extends\\s*.*",
      // this additional filter can be used to filter specifications names
      filter = { (name: String) => true },
      // this is the base directory where specifications must be searched
      // in this setup we need to skip the current run directory which is .jvm
      basePath = DirectoryPath.unsafe(new java.io.File("../src/test/scala/examples/integration").getAbsolutePath),
      // change the verbosity to better understand how the search is
      verbose = true
    )
    .unsafeRun
