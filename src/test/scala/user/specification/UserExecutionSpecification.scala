package user
package specification
import org.specs2.execute._

class UserExecutionSpecification extends org.specs2.SpecificationWithJUnit {
  def is = "ex1" ! Failure("fail")
}
