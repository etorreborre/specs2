package org.specs2
package control

class LocalVariablesSpec extends SpecificationWithJUnit with LocalVariables {
  val examples = 
  "the setTemporarily function can be used to give a temporary value to an attribute" ! {
    var flag = true
    setTemporarily(flag, false, (b:Boolean) => flag = b) {
      flag must_== false
    }
    flag must_== true
  }^
  "the setTemporarily function can be used to give a temporary value to 2 attributes" ! {
    var (flag, flag2) = (true, "hello")
    setTemporarily(flag, false, (b:Boolean) => flag = b,
                   flag2, "world", (s:String) => flag2 = s) {
      flag must_== false
      flag2 must_== "world"
    }
    flag must_== true
    flag2 must_== "hello"
  }^
  "the setTemporarily function can be used to give a temporary value to 3 attributes" ! {
    var (flag, flag2, flag3) = (true, "hello", 1)
    setTemporarily(flag, false, (b:Boolean) => flag = b,
                   flag2, "world", (s:String) => flag2 = s,
                   flag3, 2, (i:Int) => flag3 = i) {
      flag must_== false
      flag2 must_== "world"
      flag3 must_== 2
    }
    flag must_== true
    flag2 must_== "hello"
    flag3 must_== 1
  }
}
