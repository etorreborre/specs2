package org.specs2
package scalacheck

import execute.AsResult

class ScalaCheckMatchersContextSpec extends Specification with ScalaCheck2 { def is = s2"""

 Contexts can be used to setup/teardown state when executing a Prop
   A before context
   ${ var n = 0; AsResult(prop(f1).before(n += 1)); n === 100 }
   An after context
   ${ var n = 0; AsResult(prop(f1).after(n += 1)); n === 100 }
   A before / after context
   ${ var n = 0; AsResult(prop(f1).beforeAfter(n += 1, n += 1)); n === 200 }
   An around context
   ${ var n = 0; AsResult(prop(f1).around(r => {n += 1; r})); n === 100 }
   The arguments can be used to prepare the environment
   ${ var n = 0; AsResult(prop(f1).prepare(i => {n += 1; i})); n === 100 }
   ${ var n = 0; AsResult(prop(f2).prepare((i, j) => {n += 1; (i, j) })); n === 100 }

"""
  
  val f1 = (i: Int) => true
  val f2 = (i: Int, j: Int) => true
}
