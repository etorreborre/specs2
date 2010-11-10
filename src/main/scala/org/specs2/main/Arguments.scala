package org.specs2
package main

import control.Exceptions._

private[specs2]  
case class Arguments (
  _ex:         Option[String]  = None,
  _xonly:      Option[Boolean] = None,
  _plan:       Option[Boolean] = None,
  _failtrace:  Option[Boolean] = None,
  _color:      Option[Boolean] = None,
  _noindent:   Option[Boolean] = None,
  _specName:   Option[String]  = None,
  _sequential: Option[Boolean] = None,
  _threadsNb:  Option[Int]     = None
) {
  def ex: String                = _ex.getOrElse(".*")
  def xonly: Boolean            = _xonly.getOrElse(false)
  def plan: Boolean             = _plan.getOrElse(false)
  def failtrace: Boolean        = _failtrace.getOrElse(false)
  def color: Boolean            = _color.getOrElse(false)
  def noindent: Boolean         = _noindent.getOrElse(false)
  def specName: String          = _specName.getOrElse(".*Spec")
  def sequential: Boolean       = _sequential.getOrElse(false)
  def threadsNb: Int            = _threadsNb.getOrElse(4)
  
  def overrideWith(other: Arguments) = {
    new Arguments(
      other._ex         .orElse(_ex),
      other._xonly      .orElse(_xonly),
      other._plan       .orElse(_plan),
      other._failtrace  .orElse(_failtrace),
      other._color      .orElse(_color),
      other._noindent   .orElse(_noindent),
      other._specName   .orElse(_specName),
      other._sequential .orElse(_sequential),
      other._threadsNb  .orElse(_threadsNb)
    )
  }
  override def toString = {
    "Arguments(" +
    "ex"         +" = "+ _ex         +", "+
    "xonly"      +" = "+ _xonly      +", "+
    "plan"       +" = "+ _plan       +", "+
    "failtrace"  +" = "+ _failtrace  +", "+
    "color"      +" = "+ _color      +", "+
    "noindent"   +" = "+ _noindent   +", "+
    "specName"   +" = "+ _specName   +", "+
    "sequential" +" = "+ _sequential +", "+
    "threadsNb"  +" = "+ _threadsNb  +
    ") "
    
  }
} 

private[specs2]  
object Arguments {
  
  /** @return new arguments from command-line arguments */
  def apply(implicit arguments: String*): Arguments = {
    extract(arguments)
  }
  private def extract(implicit arguments: Seq[String]): Arguments = {
    new Arguments (
       _xonly      = bool("xonly"),
       _plan       = bool("plan"),
       _failtrace  = bool("failtrace"),
       _color      = bool("color"),
       _specName   = value("specName"),
       _sequential = bool("sequential"),
       _threadsNb  = int("threadsNb")
    )
  }
  
  private def bool(name: String)(implicit args: Seq[String]): Option[Boolean] = {
    args.find(_.toLowerCase.contains(name.toLowerCase)).map(a => true)
  }
  private def value(name: String)(implicit args: Seq[String]): Option[String] = {
    args.zip(args.drop(1)).find(_._1.toLowerCase.contains(name.toLowerCase)).map(_._2)
  }
  private def int(name: String)(implicit args: Seq[String]): Option[Int] = {
    tryo(value(name)(args).map(_.toInt).get)
  }
}

private[specs2]
trait ArgumentsArgs {
  /** shorthand method to create an Arguments object */
  def args(  
    ex:         =>String   = error(""),
    xonly:      =>Boolean  = error(""),
    plan:       =>Boolean  = error(""),
    failtrace:  =>Boolean  = error(""),
    color:      =>Boolean  = error(""),
    noindent:   =>Boolean  = error(""),
    specName:   =>String   = error(""),
    sequential: =>Boolean  = error(""),
    threadsNb:  =>Int      = error("") 
  ) = new Arguments(
     tryo(".*"+ex+".*"), 
     tryo(xonly), 
     tryo(plan), 
     tryo(failtrace), 
     tryo(color), 
     tryo(noindent), 
     tryo(specName), 
     tryo(sequential), 
     tryo(threadsNb)
  )
  /** 
   * @return arguments for a literate specification: no auto indent and a sequential
   *         execution
   */
  def literate = args(noindent = true, sequential = true)  
}
private[specs2]
object ArgumentsArgs extends ArgumentsArgs

