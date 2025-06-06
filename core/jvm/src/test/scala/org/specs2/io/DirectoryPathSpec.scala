package org.specs2
package io

import java.io.File
import java.net.URI

import matcher.*
import Matcher.{given}
import FileName.*
import scala.annotation.*

class DirectoryPathSpec extends Spec with TypedEqual {
  def is = s2"""

 Paths are of 2 sorts:

  - directory paths
  - file paths

 The essential difference is only a Directory path can be appended with a path name and become a FilePath.

 Directory Paths
 ===============

 A DirectoryPath can be created from
   a String
   ${DirectoryPath.unsafe("hello/world").path === "hello/world"}
   a File
   ${DirectoryPath.unsafe(new File("hello/world")).path === "hello/world"}
   a URI
   ${DirectoryPath.unsafe(new URI("hello/world")).path === "hello/world"}

 The DirectoryPath loses its scheme if created from a string/file/uri
   ${DirectoryPath.unsafe(new URI("file://hello/world")).path === "/hello/world"}

 A DirectoryPath can be used with a Windows path
   ${DirectoryPath.unsafe("C:\\hello\\world", separator = "\\").path === "C:\\hello\\world"}

 An absolute dir path can be built from
   a string starting with a /
   ${DirectoryPath.unsafe("/hello/world").isAbsolute}
   the DirectoryPath.Root object
   ${(DirectoryPath.Root / "world").isAbsolute}

 A relative dir path can be built from
   a string not starting with a /
   ${DirectoryPath.unsafe("hello/world").isRelative}
   the DirectoryPath.Empty object
   ${(DirectoryPath.EMPTY / "world").isRelative}
   a literal string
   ${("hello" / "world").isRelative}

 Basic operations can be executed on a DirectoryPath
   get the parent
   ${DirectoryPath.Root.parent must beNone}
   ${DirectoryPath("test").parent must beSome(DirectoryPath.EMPTY)}
   ${DirectoryPath("test").asAbsolute.parent must beSome(DirectoryPath.Root)}
   ${("test" / "hello" / "world").parent must beSome("test" / "hello")}

   get the basename
   ${("test" / "hello" / "world").name === FileName.unsafe("world")}

   get the rootname
   ${("test" / "hello" / "world").root must ===(DirectoryPath("test"))}

   get the path as a string
   ${DirectoryPath.Root.path must ===("/")}
   ${DirectoryPath.EMPTY.path must ===("")}
   ${DirectoryPath("test").path must ===("test")}
   ${DirectoryPath("test").asAbsolute.path must ===("/test")}
   ${("test" / "hello" / "world").path must ===("test/hello/world")}

   get the path as a string, with a last slash
   ${DirectoryPath.Root.dirPath must ===("/")}
   ${DirectoryPath("test").dirPath must ===("test/")}
   ${("test" / "hello" / "world").dirPath must ===("test/hello/world/")}

   get a portion of the path
   ${("test" / "hello" / "world" / "eric").relativeTo("test" / "hello") === "world" / "eric"}
   ${("test" / "hello" / "world" / "eric").relativeTo("other" / "hello") === "test" / "hello" / "world" / "eric"}
   ${("test" / "hello" / "world" / "eric").relativeTo("test" / "other") === "test" / "hello" / "world" / "eric"}
   ${("test" / "hello" / "world" / "eric").relativeTo("test" / "hello") must beRelative}
   ${("test" / "hello" / "world" | "eric").relativeTo("test" / "hello") must beRelative}
   ${("test" / "hello" / "world").fromRoot === "hello" / "world"}

 FilePaths
 =========

 A FilePath can be created from
   a String
   ${FilePath.unsafe("hello/world").path === "hello/world"}
   a File
   ${FilePath.unsafe(new File("hello/world")).path === "hello/world"}
   a URI
   ${FilePath.unsafe(new URI("hello/world")).path === "hello/world"}

   get the path as a string
   ${FilePath("test").path must ===("test")})
   ${("test" / "hello" | "world").path must ===("test/hello/world")})

"""

  def beRelative: Matcher[DirectoryPath] = { (dirPath: DirectoryPath) =>
    (dirPath.isRelative, s"$dirPath is not relative")
  }

  def beAbsolute: Matcher[DirectoryPath] = { (dirPath: DirectoryPath) =>
    (dirPath.isAbsolute, s"$dirPath is not absolute")
  }

  @targetName("beRelativeFilePath")
  def beRelative: Matcher[FilePath] = { (filePath: FilePath) =>
    (filePath.isRelative, s"$filePath is not relative")
  }

  @targetName("beAbsoluteFilePath")
  def beAbsolute: Matcher[FilePath] = { (filePath: FilePath) =>
    (filePath.isAbsolute, s"$filePath is not absolute")
  }

}
