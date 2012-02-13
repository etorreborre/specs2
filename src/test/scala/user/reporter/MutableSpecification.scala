package user.reporter
import org.specs2.mutable._

class MutableSpecification extends Specification {
  "This example has a failure" in {
    "this is some padding"
    1 must_== 2
    success
  }
}

class MutableScalaCheckSpecification extends Specification with org.specs2.ScalaCheck {
  "test" >> check { i: Int =>
    i === (i+1)
  }
}

class MutableMockitoSpecification extends Specification with org.specs2.mock.Mockito {
  "test" >> {
    there was one(mock[java.util.List[Int]]).get(0)
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