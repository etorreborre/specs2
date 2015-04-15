package org.specs2
package matcher

import execute._
import Typecheck._

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
 ${ ptc"5..9&#" must not succeed }
 ${ parseAndTypecheck("""tc"5..9&#"""") must not succeed }

"""

}
