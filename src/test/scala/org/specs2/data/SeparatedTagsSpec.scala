package org.specs2
package data

import mutable.Specification
import matcher.DataTables

class SeparatedTagsSpec extends Specification with DataTables {

  "tagged elements can be included / excluded by tags" >> {
    "element tags" || "include"    | "exclude"  | "keep" |>
    "t1"           !! "t1,t2"      ! ""         ! true   |
    "t1,t2"        !! "t1"         ! ""         ! true   |
    "t1,t2"        !! "t3"         ! ""         ! false  |
    "t1,t2"        !! "t1 && t2"   ! ""         ! true   |
    "t1"           !! "t1 && t2"   ! ""         ! false  |
    "t1"           !! ""           ! "t1,t2"    ! false  |
    "t1,t2"        !! ""           ! "t1"       ! false  |
    "t1,t2"        !! ""           ! "t3"       ! true   |
    "t1,t2"        !! ""           ! "t1 && t2" ! false  |
    "t1"           !! ""           ! "t1 && t2" ! true   |
    { (elementTags, included, excluded, keep) =>
      SeparatedTags(included, excluded).keep(elementTags.split(",")) === keep
    }
  }

  "it is possible to know if a tag is contained in the tags to include - tags to exclude" >> {
      "element tags" || "include"    | "exclude"  | "contain" |>
      "t1"           !! "t1,t2"      ! ""         ! true      |
      "t1,t2"        !! "t1"         ! ""         ! true      |
      "t1,t2"        !! "t3"         ! ""         ! false     |
      "t1,t2"        !! "t1 && t2"   ! ""         ! true      |
      "t1"           !! "t1 && t2"   ! ""         ! true      |
      "t1"           !! ""           ! "t1,t2"    ! false     |
      "t1,t2"        !! ""           ! "t1"       ! false     |
      "t1,t2"        !! ""           ! "t3"       ! true      |
      "t1,t2"        !! ""           ! "t1 && t2" ! false     |
      "t1"           !! ""           ! "t1 && t2" ! false     |
      { (elementTags, included, excluded, contain) =>
        SeparatedTags(included, excluded).contain(elementTags.split(",")) === contain
      }
  }
}
