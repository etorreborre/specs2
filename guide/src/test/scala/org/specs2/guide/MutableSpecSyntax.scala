package org.specs2.guide

object MutableSpecSyntax extends UserGuidePage { def is = "Mutable specification syntax".title ^ s2"""

The $Structure page presents one syntax for declaring examples in a mutable specification:${snippet{
class MySpecification extends org.specs2.mutable.Specification {
  "this is my specification" >> {
    "where example 1 must be true" >> {
      1 must_== 1
    }
    "where example 2 must be true" >> {
      2 must_== 2
    }
  }
}
}}

You can also use the `should/in` syntax:${snippet {
class MySpecification extends org.specs2.mutable.Specification {
  "this is my specification" should {
    "have one example" in {
      1 must_== 1
    }
    "and another one" in {
      2 must_== 2
    }
  }
}
}}

$warn You might get clashes with `should` which can also be used to declare expectations on strings:
```
class MySpecification extends org.specs2.mutable.Specification {
  "this" should {
    "will not compile because should is overloaded" in  {
      "a string" should not be empty
    }
  }
}
```

The easiest work around in that case is to use `must` for the expectation:${snippet{
class MySpecification extends org.specs2.mutable.Specification {
  "this" should {
    "compile now" in {
      "a string" must not be empty
    }
  }
}}}

"""

}
