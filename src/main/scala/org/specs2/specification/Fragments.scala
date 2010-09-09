package org.specs2
package specification

trait Fragment	
case class Text(t: String) extends Fragment
case class Group(fragments: List[Fragment])

object end extends Fragment
object par extends Fragment
object br extends Fragment
