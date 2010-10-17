package org.specs2
package form

class FormsSpec extends SpecificationWithJUnit with Forms {
  def is = 
                                                                                          """
  The Forms object provides several utility functions for creating forms
                                                                                          """                                                                                       ^
"  The subset method allows to check if a list of forms is a subset of another one"       ^
"    subset(l1, l1 + l2) == l1 - ok"                                                      ! subset.e1^
"    subset(l1, l2) == l1 - ko"                                                           ! subset.e2^
"    subset(l1 + l2, l1) == l1 - ok + l2 - ko"                                            ! subset.e3^
                                                                                          p^
"  If the subset method fails, the form are shown as failed"                              ^
"    subset(l1, l1 + l2) == l1 - ok"                                                      ! subset.e4^
"    subset(l1, l2) == l1 - ko"                                                           ! subset.e5^
"    subset(l1 + l2, l1) == l1 - ok + l2 - ko"                                            ! subset.e6^
                                                                                          p^
"  The subsequence method allows to check if a list of forms is a subsequence "           +
"  of another one"                                                                        ^ 
"    subsequence(ab, ab + cd) == ab - ok"                                                 ! subsequence.e1^
"    subsequence(abc, bac + d) == ab - ko + c - ok"                                       ! subsequence.e2^
"    subsequence(ab, cd) == ab - ko"                                                      ! subsequence.e3^
"    subsequence(ab + cd, ab) == ab - ok + cd - ko"                                       ! subsequence.e4^
"    subsequence(ba + cd, ab) == ba - ko + cd - ko"                                       ! subsequence.e5^
                                                                                          p^
"  The set method allows to check if 2 lists of forms are the same, in no specific order" ^
"    set(l1, l1 + l2) == l1 - ok + l2 ko"                                                 ! set.e1^
"    set(l1 + l2, l2) == l1 - ko + l2 ok"                                                 ! set.e2^
"    set(l1, l2) == l1 - ko"                                                              ! set.e3^
"    set(l1, l1) == l1 - ok"                                                              ! set.e4^
                                                                                          p^
"  The sequence method allows to check if 2 lists of forms are the same, in order"        ^
"    sequence(ab, ab + cd) == ab - ok + cd - ko"                                          ! sequence.e1^
"    sequence(ab, ba) == ab - ko"                                                         ! sequence.e2^
"    sequence(ab, ba + c) == a - ok + bc - ko"                                            ! sequence.e3^
"    sequence(abc, ba) == a - ok + bc - ko"                                               ! sequence.e4^
                                                                                          end

  def sameExecution(f1: List[Form], f2: List[Form]) = f1.map(_.execute.message) must_== f2.map(_.execute.message)
  
  object subset {
    val set1 = List(Form.tr("a"), Form.tr("b"))
    val set2 = List(Form.tr("c"), Form.tr("d"))
    
    def e1 = executeSubset(set1, set1 ++ set2) must_== set1.map(_.setSuccess)
    def e2 = executeSubset(set1, set2).map(_.text) must_== set1.map(_.text) 
    def e3 = executeSubset(set1 ++ set2, set1).map(_.text) must_== set1.map(_.text) ++ set2.map(_.text)
    
    def e4 = executeSubset(set1, set1 ++ set2).forall(_.isSuccess) must beTrue
    def e5 = executeSubset(set1, set2).forall(_.isSuccess)  must_== false
    def e6 = executeSubset(set1 ++ set2, set1).exists(_.isSuccess) &&  
             executeSubset(set1 ++ set2, set1).exists(!_.isSuccess) must beTrue
  }

  val a = List(Form.tr("a"))
  val b = List(Form.tr("b"))
  val ab = List(Form.tr("a"), Form.tr("b"))
  val ba = List(Form.tr("b"), Form.tr("a"))
  val bc = List(Form.tr("b"), Form.tr("c"))
  val abc = List(Form.tr("a"), Form.tr("b"), Form.tr("c"))
  val bac = List(Form.tr("b"), Form.tr("a"), Form.tr("c"))
  val cd = List(Form.tr("c"), Form.tr("d"))
  val c = List(Form.tr("c"))
  val d = List(Form.tr("d"))

  object subsequence {
    
    def e1 = sameExecution(executeSubsequence(ab, ab ++ cd), ab.map(_.setSuccess))
    def e2 = sameExecution(executeSubsequence(abc, bac ++ d), ab.map(_.setFailure) ++ c.map(_.setSuccess)) 
    def e3 = sameExecution(executeSubsequence(ab, cd), ab.map(_.setFailure))
                         
    def e4 = sameExecution(executeSubsequence(ab ++ cd, ab), ab.map(_.setSuccess) ++ cd.map(_.setFailure))
    def e5 = sameExecution(executeSubsequence(ba ++ cd, ab), ba.map(_.setFailure) ++ cd.map(_.setFailure))
  }

  object set {
    val set1 = List(Form.tr("a"), Form.tr("b"))
    val set2 = List(Form.tr("c"), Form.tr("d"))
    
    def e1 = sameExecution(executeSet(set1, set1 ++ set2), set1.map(_.setSuccess) ++ set2.map(_.setFailure))
    def e2 = sameExecution(executeSet(set1 ++ set2, set2), set2.map(_.setSuccess) ++ set1.map(_.setFailure))
    def e3 = sameExecution(executeSet(set1, set2), set1.map(_.setFailure) ++ set2.map(_.setFailure))
    def e4 = sameExecution(executeSet(set1, set1), set1.map(_.setSuccess))
  }
  
  object sequence {
    
    def e1 = sameExecution(executeSequence(ab, ab ++ cd), ab.map(_.setSuccess) ++ cd.map(_.setFailure))
    def e2 = sameExecution(executeSequence(ab, ba), ab.map(_.setFailure)) 
    def e3 = sameExecution(executeSequence(ab, ba ++ c), bac.map(_.setFailure))
    def e4 = sameExecution(executeSequence(abc, ba), abc.map(_.setFailure))
  }
}