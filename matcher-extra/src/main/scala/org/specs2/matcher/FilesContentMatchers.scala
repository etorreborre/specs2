package org.specs2
package matcher

import java.io.File
import text._
import io._
import Paths._
import MatchersImplicits._

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
    val (md5_1, md5_2) = (md5(actual), md5(expected))
    val message = TextTable(header = Seq("file", "MD5"), lines = Seq(Seq(actual.getPath, md5_1), Seq(expected.getPath, md5_2))).show
    (md5_1 == md5_2, s"MD5 mismatch:\n$message")
  }

  /** match 2 files if they have the same MD5 digest */
  def haveSameMD5As(expected: File): Matcher[File] = { actual: File =>
    haveSameMD5(createExpectable((actual, expected)))
  }

  case class LocalPathsMatcher(expectedDir: File, filter: File => Boolean = (f: File) => true) extends Matcher[File] {
    def apply[S <: File](actualDir: Expectable[S]) = {
      result(haveSameLinesAs(LocalPaths(expectedDir, filter)).apply(createExpectable(LocalPaths(actualDir.value, filter))), actualDir)
    }

    def withFilter(f: File => Boolean) = copy(filter = f)
  }

  case class LocalFilesContentMatcher(expectedDir: File,
                                      filter: File => Boolean = (f: File) => true,
                                      filesMatcher: Matcher[(File, File)] = haveSameLines[File, File]) extends Matcher[File] {
    def apply[S <: File](actualDir: Expectable[S]) = {
      val expectedFiles = LocalPaths(expectedDir, filter)
      val pairs = expectedFiles.localPaths.map(p => (new File(actualDir.value.getPath.dirPath+p), new File(expectedDir.getPath.dirPath+p))).filter(_._1.exists)
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
    def name(lp: LocalPaths) = lp.basePath
    def lines(lp: LocalPaths) = lp.localPaths
  }

  private case class LocalPaths(base: File, filter: File => Boolean = (f: File) => true) {
    def basePath = base.getPath
    def files = recurse(base).filter(filter).toSeq.sortBy(fromBaseFile(base))
    def localPaths = files.map(fromBaseFile(base)).sorted.toSeq
  }

  /**
   * collect files recursively, including directories
   */
  private def recurse(file: File): Stream[File] = {
    val files = file.listFiles
    if (files == null) Stream.empty
    else files.toStream ++ files.filter(_.isDirectory).toStream.flatMap(recurse)
  }

}
