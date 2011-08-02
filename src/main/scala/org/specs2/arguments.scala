package specs2

import org.specs2.text.AnsiColors
import org.specs2.control.StackTraceFilter
import org.specs2.main.{ArgProperties, ArgProperty, Arguments, Diffs}
import org.specs2.reporter.Colors

/**
 * import arguments._ to get access to all the Arguments creation functions
 */
object arguments extends org.specs2.main.ArgumentsArgs with ArgProperties