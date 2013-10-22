package user.reporter

class OkSpecification extends org.specs2.mutable.Specification {
  "This example is ok" in ok
}
class KoSpecification extends org.specs2.mutable.Specification {
  "This example is ko" in ko
}
class ErrorSpecification extends org.specs2.mutable.Specification {
  "This example has an error" in { throw new Exception("bang"); ok }
}
