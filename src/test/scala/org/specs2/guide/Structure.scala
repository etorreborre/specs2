package org.specs2
package guide

import structure._

class Structure extends UserGuidePage { def is = s2"""

### Presentation

In this section you will learn how to:

 * create examples and expectations
 * define contexts to execute actions before/after examples
 * structure a group of specifications by using links
 * specify the execution strategy
 * work with Unit specifications

 ${Examples.section  }
 ${Contexts.section  }
 ${Links.section     }
 ${Execution.section }
 ${Unit.section      }
  ----
  """ ^
  link(new FragmentsApi) ^
  link(new GivenWhenThenPage)
}
