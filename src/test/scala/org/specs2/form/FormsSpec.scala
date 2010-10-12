package org.specs2
package form

class FormsSpec extends SpecificationWithJUnit with Forms {
  val content = 
"""
  The Forms object provides several utility functions for creating forms
"""                                                                                       ^
"  The subset method allows to check if a list of forms is a subset of another one"       ^
"    subset(l1, l1 + l2) == l1 - ok"                                                      ! subset.e1^
"    subset(l1, l2) == l1 - ko"                                                           ! subset.e2^
"    subset(l1 + l2, l1) == l1 - ok + l2 - ko"                                            ! subset.e3^
                                                                                          p^
"  If there are there are failures, the form are executed as failed"                      ^
"    subset(l1, l1 + l2) == l1 - ok"                                                      ! subset.e4^
"    subset(l1, l2) == l1 - ko"                                                           ! subset.e5^
"    subset(l1 + l2, l1) == l1 - ok + l2 - ko"                                            ! subset.e6^
end

  object subset {
    val set1 = List(Form.tr("a"), Form.tr("b"))
    val set2 = List(Form.tr("c"), Form.tr("d"))
    
    def e1 = executeSubset(set1, set1 ++ set2) must_== set1.map(_.setSuccess)
    def e2 = executeSubset(set1, set2).map(_.text) must_== set1.map(_.text) 
    def e3 = executeSubset(set1 ++ set2, set1).map(_.text) must_== set1.map(_.text) ++ set2.map(_.text)
    
    def e4 = executeSubset(set1, set1 ++ set2).forall(_.isSuccess) must beTrue
    def e5 = executeSubset(set1, set2).forall(_.isSuccess) aka executeSubset(set1, set2).toString must_== false
    def e6 = executeSubset(set1 ++ set2, set1).exists(_.isSuccess) &&  
             executeSubset(set1 ++ set2, set1).exists(!_.isSuccess) must beTrue
  }
}