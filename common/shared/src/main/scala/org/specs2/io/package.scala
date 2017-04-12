package org.specs2

import reflect.MacroContext._

package object io {

  /**
   * It is possible to create a DirPath or FilePath, starting from a string "tmp" </> "dir"
   */
  implicit class NameToDirPathSyntax(name: FileName) {
    def /(other: FileName): DirectoryPath  = DirectoryPath(name) / other
    def |(other: FileName): FilePath = DirectoryPath(name) | other
  }

  /**
   * It is possible to create a FileName from a string provided it is well-formed
   */
  implicit def ToFileName(s: String): FileName =
    macro createFileName

  /**
   * create a file name from a String
   */
  def fileNameFromString(s: String): Option[FileName] =
    if (s.contains("/")) None
    else                 Some(FileName.unsafe(s))

  def createFileName(c: Context)(s: c.Expr[String]): c.Expr[FileName] = {
    import c.universe._
    s match {
      case Expr(Literal(Constant(v: String))) => createFileNameFromString(c)(v)
      case _ => c.abort(c.enclosingPosition, s"Not a literal ${showRaw(s)}")
    }
  }

  private def createFileNameFromString(c: Context)(s: String): c.Expr[FileName] = {
    import c.universe._
    fileNameFromString(s) match {
      case None     => c.abort(c.enclosingPosition, s"$s is not a valid fileName. It must not contain a /")
      case Some(fn) => c.Expr(q"FileName.unsafe(${fn.name})")
    }
  }
}
