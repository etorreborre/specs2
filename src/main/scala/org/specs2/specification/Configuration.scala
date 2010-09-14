package org.specs2
package specification
import main._

case class Configuration (
  val printStackTrace: Boolean = false,
  val failOnly: Boolean = false,
  val pendingOnly: Boolean = false) {
  
  def text = !failOnly && !pendingOnly
  def pending = !failOnly
}
object Configuration {
  def apply(args: Args) = new Configuration(
    printStackTrace = args.contains("stacktrace"),
    failOnly = args.contains("xonly"),
    pendingOnly = args.contains("pending"))
}
trait AConfiguration {
  val configuration: Configuration
}
