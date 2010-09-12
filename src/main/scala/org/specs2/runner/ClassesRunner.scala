package org.specs2
package runner
import reflect._
import io._

class ClassRunner extends Classes with ConsoleOutput {
  def main(arguments: Array[String]) = {
    if (arguments.length == 0)
      println("The first argument should be at least the specification class name")
    createSpecification(arguments(0)).main(arguments.drop(1))
  }
  def createSpecification(className: String): Specification = {
    tryToCreateObject[Specification](className, true, true) match {
      case Some(s) => s
      case None => error("can not create specification: "+className)
    }
  }
  private def fullClassName(packageName: String, className: String) = {
    if (packageName.trim.isEmpty) 
      className.trim
    else 
      packageName.trim+"."+className.trim
  }

}

