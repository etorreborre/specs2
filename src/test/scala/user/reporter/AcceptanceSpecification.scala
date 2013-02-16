package user.reporter

import org.specs2._

class AcceptanceSpecification extends Specification { def is =

  "this example succeeds" ! e1^
  "this example fails"    ! e2

  def e1 = success
  def e2 = failure
}
