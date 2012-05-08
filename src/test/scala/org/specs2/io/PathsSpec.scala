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
    "./src/com/text"    !! "./../../"     |
    "src/com/text/"     !! "./../../"     | { (a, b) => a.baseDir must_== b }
  }

  "The parent directory can be computed from a file path" >> {
    "path"              || "result"        |>
    "text"              !! "/"             |
    "src/text"          !! "src/"          |
    "src\\text"         !! "src/"          |
    "src/com/text"      !! "src/com/"      |
    "./src/com/text"    !! "src/com/"      |
    "src/com/text/"     !! "src/com/"      | { (a, b) => a.parentDir must_== b }
  }

  "A relative path can be computed from another path" >> {
    "path1"             || "path2"             || "result"              |>
    "text1"             !! "text2"             !! "text1"               |
    "guide/text1"       !! "guide/text1"       !! "../guide/text1"      |
    "src/com/text1"     !! "src/com/text2"     !! "../../src/com/text1" | { (a, b, c) => a.relativeTo(b) must_== c }
  }

  "A relative path can be uncomputed from another path" >> {
    "path1"               || "path2"             || "result"              |>
    "text1"               !! "text2"             !! "text1"               |
    "../../src/com/text1" !! "src/com/text2"     !! "src/com/text1" | { (a, b, c) => a.unrelativeTo(b) must_== c }
  }

  "A relative path can be specified as if coming from the top of its own path" >> {
    "path1"               || "result"              |>
    "text1"               !! "text1"               |
    "src/com/text1"       !! "../../src/com/text1" | { (a, b) => a.fromTop must_== b }
  }

  "uriEncode encodes a String with special URI characters so that it can be used as an html link" >> {
    "a page".uriEncode === "a%20page"
  }
}
