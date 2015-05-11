package org.specs2
package matcher

import execute._
import Typecheck._
import TypecheckMatcherMacros._

class TypecheckMatcherSpec extends Specification with TypecheckMatchers with ResultMatchers { def is = sequential ^ s2"""

 typechecking code
 ${ typecheck("val a = 1") must succeed }
 ${ typecheck("val a: String = 1") must not succeed }
 ${ typecheck("val a = b") must not succeed }
 ${ typecheck("val a: String = 1") must failWith("type mismatch") }
 ${ typecheck("val a = b").pendingUntilFixed }

 typechecking code with a quasiquote
 ${ tc" 1 must_== 1 " }
 ${ tc" 1 must_== 2 " must beFailing }
 // this needs some investigation because it
 // doesn't  compile in a multi-module build
 $${ tcw" 1 must_== 2 "(Typecheck.parsingAtRuntime) must beFailing }

 typechecking but error on missing implicits at compile-time

 typecheck but error on macro-expansion at compile-time
 ${typecheck("""typecheckWith(macrosAtCompileTime)("produceIncorrectCode")""") must not succeed}
 ${typecheck("""typecheckWith(macrosAtCompileTime)("produceCorrectCode")""") must succeed}
 ${typecheck("""typecheckWith(macrosAtCompileTime <| implicitsAtCompileTime)("produceCorrectCode")""") must succeed}
 ${typecheck("produceIncorrectCode") must not succeed }
 ${typecheckWith(parsingAtRuntime)("""tc"5..9&#"""") must not succeed }

 typecheck but error on missing implicits at compile-time
 ${typecheck("""typecheckWith(implicitsAtCompileTime)("typelevelFun(5)")""") must not succeed}
 ${typecheck("""typecheckWith(macrosAtCompileTime <| implicitsAtCompileTime)("typelevelFun(5)")""") must not succeed}
 ${typecheck("""typecheckWith(implicitsAtCompileTime)("5")""") must succeed}
 ${typecheck("produceIncorrectCode") must not succeed }

"""

 trait Evidence[A]
 def typelevelFun[A: Evidence](a: A) = a
}

