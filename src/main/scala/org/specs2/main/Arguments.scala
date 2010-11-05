package org.specs2
package main

private[specs2]
trait ArgumentsArgs {
    /** shorthand method to create an Arguments object */
  def args(  
     ex: String                      = ".*" 
    ,xonly: Boolean                  = Arguments().xonly 
    ,printStackTrace: Boolean        = Arguments().printStackTrace
    ,specName: String                = Arguments().specName
  ) = Arguments(".*"+ex+".*", xonly, printStackTrace, specName)
}
private[specs2]
object ArgumentsArgs extends ArgumentsArgs

private[specs2]  
case class Arguments (
  ex: String                       = ".*" 
  ,xonly: Boolean                  = false 
  ,printStackTrace: Boolean        = true
  ,specName: String                = ".*Spec"
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
      ,specName= value("specName", defaults.specName)
    )
  }
  
  private def bool(name: String, defaultValue: Boolean)(implicit args: Seq[String]) = {
    args.find(_.toLowerCase.contains(name.toLowerCase)).map(a => true).getOrElse(defaultValue)
  }
  private def value(name: String, defaultValue: String)(implicit args: Seq[String]) = {
    args.zip(args.drop(1)).find(_._1.toLowerCase.contains(name.toLowerCase)).map(_._2).getOrElse(defaultValue)
  }
}
