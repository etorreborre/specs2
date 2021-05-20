package specs2

import org.specs2.runner.FilesRunner

/**
 * Run specification files from the command line with specs2.files <specification name> <arguments>
 */
object files extends FilesRunner {
  def main(args: Array[String]) =
    run(args, exit = true)
}
