package specs2

/**
 * runner ouputing results in the Console
 */
object run extends org.specs2.runner.ClassRunner

object run2  {

  def main(args: Array[String]) { new org.specs2.runner.ClassRunner{}.main(args) }
}
