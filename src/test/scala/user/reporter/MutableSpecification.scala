package user.reporter
import org.specs2.mutable._

class MutableSpecification extends Specification {
  "This example has a failure" in {
    "this is some padding"
    1 must_== 2
    success
  }
}