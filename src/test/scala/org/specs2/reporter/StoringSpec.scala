package org.specs2
package reporter

class StoringSpec { def is  =

  "The statistics of a specification must be computed and stored" ! store().e1^
                                                                  end

  case class store() {
    def e1 = pending
  }

}