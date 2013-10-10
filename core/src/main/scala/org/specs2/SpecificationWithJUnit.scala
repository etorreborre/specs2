package org.specs2

import org.junit.runner._
import runner._

/**
 * This class must be inherited to allow a Specification to be executed as a JUnit test
 */
@RunWith(classOf[JUnitRunner]) 
abstract class SpecificationWithJUnit extends Specification
