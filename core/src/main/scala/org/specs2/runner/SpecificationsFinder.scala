package org.specs2
package runner

import java.util.regex._
import control._
import specification.core._
import text.SourceFile._
import io._
import scalaz._, Scalaz._

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
  def findSpecifications(glob: String                   = "**/*.scala",
                         pattern: String                = ".*Spec",
                         filter: String => Boolean      = { (name: String) => true },
                         basePath: DirectoryPath        = DirectoryPath.unsafe(new java.io.File("src/test/scala").getAbsolutePath),
                         verbose: Boolean               = false,
                         classLoader: ClassLoader       = Thread.currentThread.getContextClassLoader,
                         filePathReader: FilePathReader = FileSystem,
                         env: Env                       = Env()): Action[List[SpecificationStructure]] =
    specificationNames(glob, pattern, basePath, filePathReader, verbose).flatMap { names =>
      names.toList.filter(filter).map { name =>
        SpecificationStructure.create(name, classLoader, Some(env)).map(s => Option(s)).
          orElse(warn("[warn] cannot create specification "+name).as(none[SpecificationStructure]))
      }.sequenceU.map(_.flatten)
    }

  /**
   * @param glob a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @param filter a function to filter out unwanted specifications
   * @return specifications created from specification names
   */
  def specifications(glob: String                   = "**/*.scala",
                     pattern: String                = ".*Spec",
                     filter: String => Boolean      = { (name: String) => true },
                     basePath: DirectoryPath        = DirectoryPath.unsafe(new java.io.File("src/test/scala").getAbsolutePath),
                     verbose: Boolean               = false,
                     classLoader: ClassLoader       = Thread.currentThread.getContextClassLoader,
                     filePathReader: FilePathReader = FileSystem): Seq[SpecificationStructure] =
    findSpecifications(glob, pattern, filter, basePath, verbose, classLoader, filePathReader)
      .execute(if (verbose) consoleLogging else noLogging)
      .unsafePerformIO().toEither.fold(
        e   => { if (verbose) println(e); Seq() },
        seq => seq)

  /**
   * @param pathGlob a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @return specification names by scanning files and trying to find specifications declarations
   */
  def specificationNames(pathGlob: String, pattern: String, basePath: DirectoryPath, filePathReader: FilePathReader, verbose: Boolean) : Action[List[String]] = {
    lazy val specClassPattern = {
      val p = specPattern("class", pattern)
      log("  the pattern used to match specification classes is: "+p, verbose) >>
        Actions.safe(Pattern.compile(p))
    }

    lazy val specObjectPattern = {
      val p = specPattern("object", pattern)
      log("  the pattern used to match specification objects is: "+p, verbose) >>
        Actions.safe(Pattern.compile(p))
    }

    for {
      objectPattern <- specObjectPattern
      classPattern  <- specClassPattern
      paths         <- filePathReader.filePaths(basePath, pathGlob, verbose)
    } yield paths.toList.map(path => readClassNames(path, objectPattern, classPattern, filePathReader, verbose)).sequenceU.map(_.flatten)
  }.flatMap[List[String]](identity)

  /**
   * Read the content of the file at 'path' and return all names matching the object pattern
   * or the class pattern
   */
  def readClassNames(path: FilePath, objectPattern: Pattern, classPattern: Pattern, filePathReader: FilePathReader, verbose: Boolean): Action[Seq[String]] = {
    for {
      fileContent <- filePathReader.readFile(path)
      packName    =  packageName(fileContent)
      _           <- log("Searching for specifications in file: "+path.path, verbose)
    } yield (classNames(packName, fileContent, objectPattern, "$", verbose) |@| classNames(packName, fileContent, classPattern, "", verbose))(_ ++ _)
  }.flatMap(identity)

  /**
   * pattern to use to get specification names from file contents
   */
  def specPattern(specType: String, pattern: String) = "\\s*"+specType+"\\s*(" + pattern + ")\\s*extends\\s*.*"
}

object SpecificationsFinder extends SpecificationsFinder

