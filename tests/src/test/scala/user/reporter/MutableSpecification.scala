package user.reporter
import org.specs2.mutable._

class MutableSpecification extends Specification {
  "This example has a failure" in {
    assert("this is some padding".nonEmpty)
    1 must_== 2
    success
  }
}

class MutableScalaCheckSpecification extends Specification with org.specs2.ScalaCheck {
  "test" >> prop { i: Int =>
    i === (i+1)
  }
}

class MutableMockitoSpecification extends Specification with org.specs2.mock.Mockito {
  "test" >> {
    there was one(mock[java.util.List[Int]]).get(0)
  }
}