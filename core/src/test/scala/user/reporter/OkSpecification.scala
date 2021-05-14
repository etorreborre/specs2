package user.reporter

class OkSpecification extends org.specs2.mutable.Spec:
  "This example is ok" >> ok
class KoSpecification extends org.specs2.mutable.Spec:
  "This example is ko" >> ko
class ErrorSpecification extends org.specs2.mutable.Spec:
  "This example has an error" >> { throw new Exception("bang"); ok }
