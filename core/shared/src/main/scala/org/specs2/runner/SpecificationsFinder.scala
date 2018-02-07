package org.specs2
package runner

import java.util.regex._

import control._
import specification.core._
import text.SourceFile._
import io._
import org.specs2.fp.syntax._
import SpecificationsFinder._
import control.Operations._
import org.specs2.control.eff.Eff
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
  def findSpecifications(glob: String                   = specificationsPath,
                         pattern: String                = specificationsPattern,
                         filter: String => Boolean      = { (name: String) => true },
                         basePath: DirectoryPath        = DirectoryPath.unsafe(new java.io.File(specificationsBasePath).getAbsolutePath),
                         verbose: Boolean               = false,
                         classLoader: ClassLoader       = Thread.currentThread.getContextClassLoader,
                         filePathReader: FilePathReader = FileSystem,
                         env: Env                       = Env()): Operation[List[SpecificationStructure]] =
    specificationNames(glob, pattern, basePath, filePathReader, verbose).flatMap { names =>
      names.filter(filter).map { name =>
        SpecificationStructure.create(name, classLoader, Some(env)).map(s => Option(s)).
          orElse(warn("[warn] cannot create specification "+name).as(None: Option[SpecificationStructure]))
      }.sequence.map(_.flatten)
    }

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
                     classLoader: ClassLoader       = Thread.currentThread.getContextClassLoader,
                     filePathReader: FilePathReader = FileSystem): Seq[SpecificationStructure] = {
    val logging = if (verbose) consoleLogging else noLogging
    val specs = findSpecifications(glob, pattern, filter, basePath, verbose, classLoader, filePathReader)

    val (result, warnings) = executeOperation(specs, logging)

    println(warnings.mkString("\n", "\n", "\n"))
    result.fold(e  => { e.fold(_.printStackTrace, println); Seq() }, seq => seq)
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
                         classLoader: ClassLoader       = Thread.currentThread.getContextClassLoader,
                         filePathReader: FilePathReader = FileSystem,
                         env: Env                       = Env()): Seq[Fragment] = {
    val logging = if (verbose) consoleLogging else noLogging
    import DefaultFragmentFactory._

    val links: Operation[List[Fragment]] = specificationNames(glob, pattern, basePath, filePathReader, verbose).flatMap { names =>
      names.filter(filter).traverse { name =>
        SpecificationStructure.create(name, classLoader, Some(env)).map(s => link(SpecificationRef.create(s.is))).
        whenFailed {
          case Left(t) => Eff.pure(example("cannot create specification " + name, Execution.result(org.specs2.execute.Error(t))))
          case Right(m) => Eff.pure(example("cannot create specification " + name, Execution.result(org.specs2.execute.Error(m))))
        }
      }
    }

    val (results, warnings) = executeOperation(links, logging)
    println(warnings.mkString("\n", "\n", "\n"))

    results match {
      case Left(t) => println(t); Seq()
      case Right(ss) => ss
    }

  }

  /**
   * @param pathGlob a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @return specification names by scanning files and trying to find specifications declarations
   */
  def specificationNames(pathGlob: String, pattern: String, basePath: DirectoryPath, filePathReader: FilePathReader, verbose: Boolean) : Operation[List[String]] = {
    lazy val specClassPattern = {
      val p = specPattern("class", pattern)
      log("  the pattern used to match specification classes is: "+p, verbose) >>
        Operations.delayed(Pattern.compile(p))
    }

    lazy val specObjectPattern = {
      val p = specPattern("object", pattern)
      log("  the pattern used to match specification objects is: "+p, verbose) >>
        Operations.delayed(Pattern.compile(p))
    }

    for {
      objectPattern <- specObjectPattern
      classPattern  <- specClassPattern
      paths         <- filePathReader.filePaths(basePath, pathGlob, verbose)
    } yield paths.map(path => readClassNames(path, objectPattern, classPattern, filePathReader, verbose)).sequence.map(_.flatten)
  }.flatMap[List[String]](identity)

  /**
   * Read the content of the file at 'path' and return all names matching the object pattern
   * or the class pattern
   */
  def readClassNames(path: FilePath, objectPattern: Pattern, classPattern: Pattern, filePathReader: FilePathReader, verbose: Boolean): Operation[List[String]] = {
    for {
      fileContent <- filePathReader.readFile(path)
      packName    =  packageName(fileContent)
      _           <- log("Searching for specifications in file: "+path.path, verbose)
    } yield (classNames(packName, fileContent, objectPattern, "$", verbose) |@| classNames(packName, fileContent, classPattern, "", verbose))(_ ++ _)
  }.flatMap(identity)

  /**
   * pattern to use to get specification names from file contents
   */
  def specPattern(specType: String, pattern: String) = "\\s*"+specType+"\\s*" + pattern
}

object SpecificationsFinder extends SpecificationsFinder {

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

