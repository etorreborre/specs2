package org.specs2
package runner

import java.util.regex._

import control._
import specification.core._
import text._
import io._
import org.specs2.fp.syntax._
import SpecificationsFinder._
import control._
import org.specs2.specification.create.DefaultFragmentFactory
import org.specs2.specification.create.DefaultFragmentFactory.link

/**
 * This trait loads specifications found on a given source directory based
 * on a regular expression representing the Specification name, usually .*Spec
 */
trait SpecificationsFinder {

  /**
   * @param glob a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @param filter a function to filter out unwanted specifications
   * @return specifications created from specification names
   */
  def findSpecifications(glob: String                   = SpecificationsFinder.specificationsPath,
                         pattern: String                = SpecificationsFinder.specificationsPattern,
                         filter: String => Boolean      = { (name: String) => true },
                         basePath: DirectoryPath        = DirectoryPath.unsafe(new java.io.File(specificationsBasePath).getAbsolutePath),
                         verbose: Boolean               = false,
                         classLoader: ClassLoader       = Thread.currentThread.getContextClassLoader): Operation[List[SpecificationStructure]]

  /**
   * @param glob a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object/class name extending a Specification
   * @param filter a function to filter out unwanted specifications
   * @return specifications created from specification names
   */
  def specifications(glob: String                   = "**/*.scala",
                     pattern: String                = SpecificationsFinder.specificationsPattern,
                     filter: String => Boolean      = { (name: String) => true },
                     basePath: DirectoryPath        = DirectoryPath.unsafe(new java.io.File("src/test/scala").getAbsolutePath),
                     verbose: Boolean               = false,
                     classLoader: ClassLoader       = Thread.currentThread.getContextClassLoader): Seq[SpecificationStructure] = {
    val specs = findSpecifications(glob, pattern, filter, basePath, verbose, classLoader)
    val result = specs.runOperation

    result.fold(e => { e.printStackTrace; Seq() }, seq => seq)
  }

}


case class DefaultSpecificationsFinder(env: Env) extends SpecificationsFinder {

  val logger: Logger =
    env.systemLogger

  val fileSystem: FileSystem =
    env.fileSystem

  /**
   * @param glob a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @param filter a function to filter out unwanted specifications
   * @return specifications created from specification names
   */
  def findSpecifications(glob: String                   = specificationsPath,
                         pattern: String                = specificationsPattern,
                         filter: String => Boolean      = { (name: String) => true },
                         basePath: DirectoryPath        = DirectoryPath.unsafe(new java.io.File(specificationsBasePath).getAbsolutePath),
                         verbose: Boolean               = false,
                         classLoader: ClassLoader       = Thread.currentThread.getContextClassLoader): Operation[List[SpecificationStructure]] =
    specificationNames(glob, pattern, basePath, verbose).flatMap { names =>
      names.filter(filter).traverse { name =>
        SpecificationStructure.create(name, classLoader, Some(env)).map(s => Option(s)).
          orElse(logger.warn("[warn] cannot create specification "+name).as(None: Option[SpecificationStructure]))
      }.map(_.flatten)
    }

  /**
   * @param glob a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object/class name extending a Specification
   * @param filter a function to filter out unwanted specifications
   * @return specifications links created from the found specifications. When a specification can not be instantiated
   * a failed example is created for it
   */
  def specificationLinks(glob: String                   = "**/*.scala",
                         pattern: String                = SpecificationsFinder.specificationsPattern,
                         filter: String => Boolean      = { (name: String) => true },
                         basePath: DirectoryPath        = DirectoryPath.unsafe(new java.io.File("src/test/scala").getAbsolutePath),
                         verbose: Boolean               = false,
                         classLoader: ClassLoader       = Thread.currentThread.getContextClassLoader): Seq[Fragment] = {
    import DefaultFragmentFactory._

    val links: Operation[List[Fragment]] = specificationNames(glob, pattern, basePath, verbose).flatMap { names =>
      names.filter(filter).traverse { name =>
        SpecificationStructure.create(name, classLoader, Some(env)).map(s => link(SpecificationRef.create(s.is))).
        recoverWith { t: Throwable =>
          example("cannot create specification " + name, Execution.result(org.specs2.execute.Error(t)))
        }
      }
    }

    links.runOperation match {
      case Left(t) => println(t); Seq()
      case Right(ss) => ss
    }

  }

  /**
   * @param pathGlob a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @return specification names by scanning files and trying to find specifications declarations
   */
  def specificationNames(
    pathGlob: String,
    pattern: String,
    basePath: DirectoryPath,
    verbose: Boolean) : Operation[List[String]] = {
    lazy val specClassPattern = {
      val p = specPattern("class", pattern)
      logger.info("  the pattern used to match specification classes is: "+p, verbose) >>
        Operation.delayed(Pattern.compile(p))
    }

    lazy val specObjectPattern = {
      val p = specPattern("object", pattern)
      logger.info("  the pattern used to match specification objects is: "+p, verbose) >>
        Operation.delayed(Pattern.compile(p))
    }

    for {
      objectPattern <- specObjectPattern
      classPattern  <- specClassPattern
      paths         <- fileSystem.filePaths(basePath, pathGlob, verbose)
    } yield paths.traverse(path => readClassNames(path, objectPattern, classPattern, verbose)).map(_.flatten)
  }.flatten

  /**
   * Read the content of the file at 'path' and return all names matching the object pattern
   * or the class pattern
   */
  def readClassNames(
    path: FilePath,
    objectPattern: Pattern,
    classPattern: Pattern,
    verbose: Boolean): Operation[List[String]] = {
    val sourceFile = SourceFile(logger)
    for {
      fileContent <- fileSystem.readFile(path)
      packName    =  sourceFile.packageName(fileContent)
      _           <- logger.info("Searching for specifications in file: "+path.path, verbose)
    } yield (sourceFile.classNames(packName, fileContent, objectPattern, "$", verbose) |@| sourceFile.classNames(packName, fileContent, classPattern, "", verbose))(_ ++ _)
  }.flatten

  /**
   * pattern to use to get specification names from file contents
   */
  def specPattern(specType: String, pattern: String) = "\\s*"+specType+"\\s*" + pattern
}

object SpecificationsFinder {

  val default: SpecificationsFinder =
    DefaultSpecificationsFinder(EnvDefault.default)

  /** base path for the specification files */
  val specificationsBasePath: String =
    "src/test/scala"

  /** glob pattern for the file paths inside the base path */
  val specificationsPath: String =
    "**/*.scala"

  /** Regex pattern used to capture a specification name in an object/class declaration */
  val specificationsPattern: String =
    "(.*Spec)\\s*extends\\s*.*"
}
