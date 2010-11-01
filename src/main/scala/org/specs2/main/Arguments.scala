package org.specs2
package main

case class Arguments (
  ex: String = ".*", 
  xonly: Boolean = false, 
  printStackTrace: Boolean = true,
  srcDir: String = "src/test/scala", 
  specsFilePattern: String = "*Spec.scala"
)
    
case object Arguments {
  def apply(implicit arguments: Array[String]) = {
    new Arguments (
      xonly = bool("xonly"),
      printStackTrace = bool("printStackTrace")
    )
  }
  private def bool(name: String)(implicit args: Array[String]) =  args.exists(_.contains(name))
}
