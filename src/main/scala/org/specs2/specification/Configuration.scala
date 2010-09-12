package org.specs2
package specification

case class Configuration (
  val printStackTrace: Boolean = true
)
object Configuration {
  def apply(args: Args) = new Configuration(printStackTrace = !args.contains("ns"))
}
trait AConfiguration {
  val configuration: Configuration
}
