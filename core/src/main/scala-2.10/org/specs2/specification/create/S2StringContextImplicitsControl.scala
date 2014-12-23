package org.specs2
package specification
package create

import org.specs2.main.CommandLineAsResult

/**
 * This trait is necessary to deactivate the commandLineAsResultIsInterpolatedFragment in Scala 2.10
 * This one conflict with the InterpolatedFragment implicit for =>Fragment and forbids to put step in
 * a spec
 */
trait S2StringContextImplicitsControl extends S2StringContext1