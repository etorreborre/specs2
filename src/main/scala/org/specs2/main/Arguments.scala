package org.specs2
package main

private[specs2]  
case class Arguments (
  ex: String                       = ".*" 
  ,xonly: Boolean                  = false 
  ,printStackTrace: Boolean        = true
  ,srcDir: String                  = "src/test/scala/" 
  ,specNamePattern: String         = ".*Spec"
)

private[specs2]  
case object Arguments {
  
  /** @return new arguments from command-line arguments */
  def apply(implicit arguments: String*): Arguments = {
    extract(new Arguments())(arguments)
  }
  /** 
   * @return new arguments from command-line arguments, with default values which
   *         may be different from the ones in the Arguments class 
   */
  def apply(defaults: Arguments)(implicit arguments: String*): Arguments = {
    extract(defaults)(arguments)
  }
  private def extract(defaults: Arguments)(implicit arguments: Seq[String]): Arguments = {
    new Arguments (
       xonly = bool("xonly", defaults.xonly)
      ,printStackTrace = bool("printStackTrace", defaults.printStackTrace)
      ,srcDir = value("srcDir", defaults.srcDir)
      ,specNamePattern = value("files", defaults.specNamePattern)
    )
  }
  
  private def bool(name: String, defaultValue: Boolean)(implicit args: Seq[String]) = {
    args.find(_.toLowerCase.contains(name.toLowerCase)).map(a => true).getOrElse(defaultValue)
  }
  private def value(name: String, defaultValue: String)(implicit args: Seq[String]) = {
    args.zip(args.drop(1)).find(_._1.toLowerCase.contains(name.toLowerCase)).map(_._2).getOrElse(defaultValue)
  }
}
