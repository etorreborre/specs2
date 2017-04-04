package org.specs2
package mutable

import specification.dsl.mutable.{ExampleDsl, TagDsl}

class SpecSpec extends Spec with TagDsl {
  sequential
  section("travis")

  "A mutable Spec specification contains very few implicits" >> {
    import scala.reflect.runtime.universe._
    typeOf[Spec].members.filter(m => m.isImplicit && m.isMethod).map(_.name.toString).toSet must
      beEqualTo(Set(
        "theValue",             // must expectations
        "blockExample",         // create >> examples
        "matcherIsValueCheck",  // to use ValueChecks in contain matchers
        "functionIsValueCheck"
      ))
  }

  "A mutable Spec can use additional traits to expand its dsl" >> {
    val s = new Spec with ExampleDsl {
      "hello" >> ok
    }
    ok
  }

}
