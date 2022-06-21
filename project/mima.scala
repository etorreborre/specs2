import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object Mima {

  lazy val excluded = Seq(
    ProblemFilters.exclude[ReversedMissingMethodProblem]("org.specs2.specification.dsl.mutable.ReferenceDsl.org$specs2$specification$dsl$mutable$ReferenceDsl$$super$~"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("org.specs2.specification.dsl.mutable.ReferenceDsl.org$specs2$specification$dsl$mutable$ReferenceDsl$$super$~/")
    )
}
