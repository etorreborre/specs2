package user
package specification

class UserFromSourceScalaCheckSpecification extends org.specs2.Specification with org.specs2.ScalaCheck { def is = noindent^
                                                                                      p^
  `a call to an example`                                                              ^
  prop { (a: String) =>
    a.size === a.size
  }^
  { "a normal example" ==> ok }^
                                                                                      end

  def `a call to an example` = prop { (i: Int) => true }
}

