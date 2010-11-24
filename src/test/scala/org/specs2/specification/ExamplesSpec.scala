package org.specs2
package specification

class ExamplesSpec extends SpecificationWithJUnit {  def is =
                                                                                          """
  In a Specification, the content variable stores an instance of the Fragments class,
  which is merely a list of fragments. Those fragments are either:
  
   * Text elements
   * Example elements, with an executable block returning
   * SpecificationFragments elements which are Fragments coming from another 
     specification
                                                                                          """^
                                                                                          p^
  "an example is simply created with string ! e1 where e1 returns a Result"               ! success^
  "an example is can also take its own description to return a Result"                    ! e1^
                                                                                          end
                                                                                        
  def e1 = {
  	val ex = "name: eric, age: 18" ! function
  	ex.body().isSuccess must beTrue
  }
  def function = (s: String) => {
    val Exp = "name: (\\w*), age: (\\d*)".r
    val Exp(name, age) = s
    (name must_== "eric") and (age must_== "18")
  }
}