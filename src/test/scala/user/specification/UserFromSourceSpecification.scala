package user
package specification

class UserFromSourceSpecification extends org.specs2.Specification { def is = noindent^
                                                                                      p^
  "this is a one-liner"                                                               ^
  { 1 must_== 1 }                                                                     ^
  "this is an example on several lines"                                               ^
  { val a = "hello"
    val b = " world"
    (a + b) must_== "hello world"
  }                                                                                   ^
  `a call to an example`                                                              ^
                                                                                      end

  def `a call to an example` = success
}

class DifferentSpecification extends org.specs2.Specification { def is = noindent^
  "first example" ^
  { 1 must_== 1 } ^
                  end
}
class SpecificationWithNoStartingTextSpec extends org.specs2.Specification { def is =
  { 1 must_== 1 } ^
  { 1 must_== 1 } ^
  { 1 must_== 1 } ^
                  end
}
class SpecificationWithNoStartingTextAndNoEndSpec extends org.specs2.Specification { def is =
  { 1 must_== 1 } ^
  { 2 must_== 2 }
}

class SpecificationWithNoStartingText extends org.specs2.Specification { def is =
  { 1 must_== 1 } ^
  { 1 must_== 1 } ^
  { 1 must_== 1 } ^
                  end
}
class SpecificationWithNoStartingTextAndNoEnd extends org.specs2.Specification { def is =
  { 1 must_== 1 } ^
  { 1 must_== 1 } ^
  { 1 must_== 1 }
}


