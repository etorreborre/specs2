package user.io
import org.specs2.Specification
import org.specs2.io.WithFragments

class LocationSpecification extends Specification with WithFragments { def is =
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

  def fragmentsList = is.fragments.fragments
}
