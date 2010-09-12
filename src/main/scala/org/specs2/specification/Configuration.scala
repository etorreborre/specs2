package org.specs2
package specification

case class Configuration (
  val printStackTrace: Boolean = true,
  val failOnly: Boolean = false
)
object Configuration {
  def apply(args: Args) = new Configuration(
    printStackTrace = !args.contains("ns"),
    failOnly = args.contains("xonly"))
}
trait AConfiguration {
  val configuration: Configuration
}
