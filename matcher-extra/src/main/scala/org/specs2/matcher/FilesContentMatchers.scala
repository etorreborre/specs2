package org.specs2
package matcher

import java.io.File
import execute.Result
import text._
import io._
import MatchersImplicits._
import scalaz.std.anyVal._
import control._

/**
 * This trait provides matchers to check the presence of some expected files vs the actual ones
 * and also ways to verify the contents of these files
 */
trait FilesContentMatchers extends FileMatchers with LinesContentMatchers with TraversableMatchers {
  /**
   * check that all the paths in `expectedDir` are the same as the ones in `actualDir`
   */
  def haveSamePathsAs(expectedDir: File) = LocalPathsMatcher(expectedDir)

  /**
   * check that all the files in `expectedDir` have the same contents as the files in `actualDir` when they are present
   * The default matcher is assuming that files are text files which must be compared line by line
   */
  def haveSameFilesContentAs(expectedDir: File) = LocalFilesContentMatcher(expectedDir)

  /**
   * check that all the paths in `expectedDir` are the same as the ones in `actualDir`
   * then check that all the files in `expectedDir` have the same contents as the files in `actualDir` when they are present
   */
  def haveSameFilesAs(expectedDir: File) = LocalPathsAndFilesContentMatcher(expectedDir)

  /** match 2 files if they have the same MD5 digest */
  def haveSameMD5: Matcher[(File, File)] = { pair: (File, File) =>
    val (actual, expected) = pair

    val action  = for {
      _     <- FilePathReader.mustExist(actual)
      _     <- FilePathReader.mustExist(expected)
      _     <- FilePathReader.mustNotBeADirectory(actual)
      _     <- FilePathReader.mustNotBeADirectory(expected)
      md5_1 <- FilePathReader.md5(FilePath.unsafe(actual))
      md5_2 <- FilePathReader.md5(FilePath.unsafe(expected))
    } yield {
      val message = TextTable(header = Seq("file", "MD5"), lines = Seq(Seq(actual.getPath, md5_1), Seq(expected.getPath, md5_2))).show
      (md5_1 == md5_2, s"MD5 mismatch:\n$message")
    }
    action.execute(noLogging).unsafePerformIO.toDisjunction.fold(
      failure => (false, Result.theseToResult(failure).message),
      identity)
  }

  /** match 2 files if they have the same MD5 digest */
  def haveSameMD5As(expected: File): Matcher[File] = { actual: File =>
    haveSameMD5(createExpectable((actual, expected)))
  }

  private def filePathFilter(filter: File => Boolean) =
    (f: FilePath) => filter(f.toFile)

  case class LocalPathsMatcher(expectedDir: File, filter: File => Boolean = (f: File) => true) extends Matcher[File] {
    def apply[S <: File](actualDir: Expectable[S]) = {
      result(haveSameLinesAs(LocalPaths(DirectoryPath.unsafe(expectedDir), filePathFilter(filter)))
        .apply(createExpectable(LocalPaths(DirectoryPath.unsafe(actualDir.value), filePathFilter(filter)))), actualDir)
    }

    def withFilter(f: File => Boolean) = copy(filter = f)
  }

  case class LocalFilesContentMatcher(expectedDir: File,
                                      filter: File => Boolean = (f: File) => true,
                                      filesMatcher: Matcher[(File, File)] = haveSameLines[File, File]) extends Matcher[File] {

    def apply[S <: File](actualDir: Expectable[S]) = {
      val expectedFiles = LocalPaths(DirectoryPath.unsafe(expectedDir), filePathFilter(filter))
      val pairs = expectedFiles.files.map { p =>
        ((DirectoryPath.unsafe(actualDir.value) </> p).toFile,
         (DirectoryPath.unsafe(expectedDir) </> p).toFile)
      }.filter(_._1.exists)
      result(contain(filesMatcher).forall.apply(createExpectable(pairs)), actualDir)
    }

    def withFilter(filter: File => Boolean) = copy(filter = filter)
    def withMatcher(m: Matcher[(File, File)]) = copy(filesMatcher = m)
  }

  case class LocalPathsAndFilesContentMatcher(expectedDir: File,
                                              filter: File => Boolean = (f: File) => true,
                                              filesMatcher: Matcher[(File, File)] = haveSameLines[File, File]) extends Matcher[File] {
    def apply[S <: File](actualDir: Expectable[S]) = {
      haveSamePathsAs(expectedDir).withFilter(filter)(actualDir) and
      haveSameFilesContentAs(expectedDir).withFilter(filter).withMatcher(filesMatcher)(actualDir)
    }

    def withFilter(filter: File => Boolean) = copy(filter = filter)
    def withMatcher(m: Matcher[(File, File)]) = copy(filesMatcher = m)
  }

  private implicit def LocalPathsLinesContent: LinesContent[LocalPaths] = new LinesContent[LocalPaths] {
    def name(lp: LocalPaths) = lp.base.path
    def lines(lp: LocalPaths) = lp.localPaths
  }

  private case class LocalPaths(base: DirectoryPath, filter: FilePath => Boolean) {
    def files = FilePathReader.listFilePaths(base).map(_.filter(filter).map(_.relativeTo(base)).sortBy(_.path)).runOption.getOrElse(Nil)
    def localPaths = files.map(_.path).sorted
  }

}
