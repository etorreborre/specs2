package org.specs2
package io
import matcher.DataTables
import Paths._

class PathsSpec extends mutable.Specification with DataTables {

  "The dirPath function must normalize directory paths" >> {
    "path"              || "result"       |>
    "src"               !! "src/"         |
    "src/"              !! "src/"         |
    "src\\"             !! "src/"         |
    "src/java"          !! "src/java/"    |
    "src/java/"         !! "src/java/"    |
    "src\\java\\"       !! "src/java/"    | { (a, b) => a.dirPath must_== b }
  }

  "The relative base directory can be computed from a file path" >> {
    "path"              || "result"       |>
    "text"              !! "./"           |
    "src/text"          !! "./../"        |
    "src\\text"         !! "./../"        |
    "src/com/text"      !! "./../../"     |
    "src/com/text/"     !! "./../../"     | { (a, b) => a.baseDir must_== b }
  }
}