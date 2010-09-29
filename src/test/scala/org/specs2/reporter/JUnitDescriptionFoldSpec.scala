package org.specs2
package reporter
import specification._

class JUnitDescriptionFoldSpec extends SpecificationWithJUnit {
  val examples = 
"""
A list of Fragments can be 'folded' into a tree of JUnit descriptions so that there is 
a root Description object (the top 'suite') and children objects representing either 
Nested suites or Tests
"""^ ""	
	
}
