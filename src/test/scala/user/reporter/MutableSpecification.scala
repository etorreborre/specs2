package user.reporter
import org.specs2.mutable._

class MutableSpecification extends Specification {
  "This example has a failure" in {
    "this is some padding"
    1 must_== 2
    success
  }
}

class OkSpecification extends Specification {
  "This example is ok" in ok
}
class KoSpecification extends Specification {
  "This example is ko" in ko
}
class ErrorSpecification extends Specification {
  "This example has an error" in { throw new Exception("bang"); ok }
}