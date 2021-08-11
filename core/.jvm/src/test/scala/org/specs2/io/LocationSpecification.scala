package user.io
import org.specs2.Specification
import org.specs2.concurrent.ExecutionEnv
import org.specs2.io.WithFragments
import org.specs2.specification.core.Fragment

class LocationSpecification(ee: ExecutionEnv) extends Specification with WithFragments { def is = s2"""
  presentation
  this block should
    have one example            $ok
    have another example        $ko

  this other block should
    have one ok example         $ok
    have one ko example         $ko
                                """

  def fragmentsList: List[Fragment] =
    is.fragmentsList(ee)
}
