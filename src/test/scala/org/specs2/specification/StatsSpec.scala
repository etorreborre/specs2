package org.specs2
package specification

import execute.Result
import matcher.DataTables._

class StatsSpec extends mutable.Specification {

  "A Stats object can be resumed to a Result" >> {
    "if there are no failures or errors -> success"  >> {

       "success" | "failure" | "error" | "pending" | "skipped" | "result"           |>
       1         ! 0         ! 0       ! 0         ! 0         ! (success: Result)  |
       0         ! 1         ! 0       ! 0         ! 0         ! failure            |
       0         ! 0         ! 1       ! 0         ! 0         ! anError            |
       0         ! 0         ! 0       ! 1         ! 0         ! pending            |
       0         ! 0         ! 0       ! 0         ! 1         ! skipped            |
       1         ! 1         ! 1       ! 1         ! 1         ! anError            |
       1         ! 1         ! 0       ! 1         ! 1         ! failure            |
       1         ! 0         ! 0       ! 1         ! 1         ! success            |
       0         ! 0         ! 0       ! 2         ! 1         ! pending            |
       0         ! 0         ! 0       ! 2         ! 2         ! skipped            |
       0         ! 0         ! 0       ! 0         ! 0         ! success            | { (s, f, e, p, sk, r) =>
         Stats(successes = s, failures = f, errors = e, pending = p, skipped = sk).result must_== r
       }
    }
  }

  "Trends" >> {
    "A trend can be computed from between 2 stats" >> {
      Stats(failures = 0).updatedFrom(Stats(failures = 1)).trend must_== Some(Stats(failures = -1))
    }
  }
  "XML" >> {
    "A Stats object can be exported as xml" >> {
      "with no trend" >> {
        Stats(1, 2, 3, 4, 5, 6, 7).toXml must be_==/(
        <stats fragments="1" successes="2" expectations="3" failures="4" errors="5" pending="6" skipped="7" time="0"></stats>)
      }
      "with a trend" >> {
        Stats(1, 2, 3, 4, 5, 6, 7, Some(Stats(-1, -2))).toXml must be_==/(
        <stats fragments="1" successes="2" expectations="3" failures="4" errors="5" pending="6" skipped="7" time="0">
          <trend><stats fragments="-1" successes="-2" expectations="0" failures="0" errors="0" pending="0" skipped="0" time="0"></stats></trend>
        </stats>)
      }
    }
    "A Stats object can be imported from xml" >> {
      "with no trend" >> {
        Stats.fromXml(
          <stats fragments="1" successes="2" expectations="3" failures="4" errors="5" pending="6" skipped="7" time="0"></stats>) must_==
        Some(Stats(1, 2, 3, 4, 5, 6, 7))
      }
      "with a trend" >> {
        Stats.fromXml(<stats fragments="1" successes="2" expectations="3" failures="4" errors="5" pending="6" skipped="7" time="0">
          <trend><stats fragments="-1" successes="-2" expectations="0" failures="0" errors="0" pending="0" skipped="0" time="0"></stats></trend>
        </stats>) must_==
        Some(Stats(1, 2, 3, 4, 5, 6, 7, Some(Stats(-1, -2))))
      }
    }
  }

}