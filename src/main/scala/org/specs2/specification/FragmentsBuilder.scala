package org.specs2
package specification

import control.LazyParameters._
import execute._
import main._
import matcher._

private[specs2]
trait FragmentsBuilder {

  implicit def seqToResult[T](r: Seq[MatchResult[T]]): Result = r.reduceLeft(_ and _).toResult
  implicit def asResult[T](r: MatchResult[T]): Result = r.toResult
  
  implicit def fragments[T <: Fragment](f: T): Fragments = Fragments(f)
  
  implicit def start(s: String): Fragments = Fragments(Text(s))
  implicit def text(s: String): Text = new Text(s)
  
  implicit def forExample(s: String): ExampleDesc = new ExampleDesc(s)
  class ExampleDesc(s: String) {
    def ![T](function: String => MatchResult[T]) = new Example(s, body = () => function(s).toResult)
	  def ![T](t: =>MatchResult[T]) = new Example(s, body = () => t.toResult)
	  def ![T <% Result](t: =>T) = new Example(s, body = () => t)
  }
  implicit def group(Fragments: Fragments) = Group(Fragments.fragments)
  implicit def group(fragments: Seq[Fragment]) = Group(fragments)

  def args(  
     ex: String                      = ".*" 
    ,xonly: Boolean                  = false 
    ,printStackTrace: Boolean        = true
    ,srcDir: String                  = "src/test/scala/" 
    ,specNamePattern: String         = ".*Spec"
  ) = Arguments(".*"+ex+".*", xonly, printStackTrace, srcDir, specNamePattern)
  
  implicit def arguments(a: Arguments) = new ArgumentsFragment(a)
  class ArgumentsFragment(a: Arguments) {
    def ^(f: Fragment) = new Fragments(() => List(f), a)
  }
}

private[specs2]
trait FragmentsShow {
  implicit object showFragments extends scalaz.Show[Fragment] {
	  def show(f: Fragment) = (f match {
	    case Example(d, _) => "Example("+d+")"
	    case other => other.toString
	  }).toList
  }
}

private[specs2]
object FragmentsShow extends FragmentsShow
