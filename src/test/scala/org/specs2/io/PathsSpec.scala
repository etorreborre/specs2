package org.specs2
package io
import matcher.DataTables
import Paths._

class PathsSpec extends Specification with DataTables { def is =

  "The dirPath function must normalize directory paths" ! {
    "path"              || "result"       |>
    "src"               !! "src/"         |
    "src/"              !! "src/"         |
    "src\\"             !! "src/"         |
    "src/java"          !! "src/java/"    |
    "src/java/"         !! "src/java/"    |
    "src\\java\\"       !! "src/java/"    | { (a, b) => a.dirPath must_== b }
  }
}