package user.io

import org.specs2.Specification

class LocationSpecification extends Specification { def is = formatSection(flow=true)^
  "presentation"                ^
  "this block should"           ^
    "have one example"          ! ok ^
    "have another example"      ! ko ^
                                p ^
  "this other block should"     ^
    "have one ok example"       !
      ok                        ^
    "have one ko example"       !
      ko                        ^
                                end

}
