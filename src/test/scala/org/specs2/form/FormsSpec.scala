package org.specs2
package form
import specification._

class FormsSpec extends Specification with FormsBuilder { def is =
                                                                                          """
  The Forms object provides several utility functions for creating forms
                                                                                          """                                                                                       ^
  "The subset method allows to check if a list of forms is a subset of another one"       ^
    "subset(l1 + l2, l1) == l1 + l2 - ok"                                                 ! subset.e1^
    "subset(l1, l2) == l1 - ko"                                                           ! subset.e2^
    "subset(l1 + l2, l1) == l1 - ok + l2 - ko"                                            ! subset.e3^
                                                                                          p^
  "If the subset method fails, the form are shown as failed"                              ^
    "subset(l1, l1 + l2) == l1 - ok"                                                      ! subset.e4^
    "subset(l1, l2) == l1 - ko"                                                           ! subset.e5^
    "subset(l1 + l2, l1) == l1 - ok + l2 - ko"                                            ! subset.e6^
                                                                                          p^
  "The subsequence method allows to check if a list of forms is a subsequence "           +
  "of another one"                                                                        ^ 
    "subsequence(ab + cd, ab) == abcd - ok"                                               ! subsequence.e1^
    "subsequence(bac + d, abc) == b - ko + a - ok + c - ko + d - ok"                      ! subsequence.e2^
    "subsequence(cd, ab) == cd - ok + ab - ko"                                            ! subsequence.e3^
    "subsequence(ab, ab + cd) == ab - ok + cd - ko"                                       ! subsequence.e4^
    "subsequence(ab, ba + cd) == ax - ko + cd - ko"                                       ! subsequence.e5^
                                                                                          p^
  "The set method allows to check if 2 lists of forms are the same, in no specific order" ^
    "set(l1, l1 + l2) == l1 - ok + l2 ko"                                                 ! set.e1^
    "set(l1 + l2, l2) == l1 - ko + l2 ok"                                                 ! set.e2^
    "set(l1, l2) == l1 - ko"                                                              ! set.e3^
    "set(l1, l1) == l1 - ok"                                                              ! set.e4^
                                                                                          p^
  "The sequence method allows to check if 2 lists of forms are the same, in order"        ^
    "sequence(ab, ab + cd) == ab - ok + cd - ko"                                          ! sequence.e1^
    "sequence(ab, ba) == ab - ko"                                                         ! sequence.e2^
    "sequence(ab, ba + c) == a - ok + bc - ko"                                            ! sequence.e3^
    "sequence(abc, ba) == a - ok + bc - ko"                                               ! sequence.e4^
                                                                                          end

  def sameExecution(f1: Seq[Form], f2: Seq[Form]) = f1.map(_.execute.message) must_== f2.map(_.execute.message)
  
  object subset {
    val set1 = List(Form.tr("a"), Form.tr("b"))
    val set2 = List(Form.tr("c"), Form.tr("d"))
    
    def e1 = FormDiffs.subset(set1 ++ set2, set2) must_== set1 ++ ok(set2)
    def e2 = FormDiffs.subset(set1, set2) must_== set1 ++ ko(set2)
    def e3 = FormDiffs.subset(set1, set1 ++ set2) must_== ok(set1) ++ ko(set2)
    
    def e4 = FormDiffs.subset(set1 ++ set2, set1).forall(_.isSuccess) must beTrue
    def e5 = FormDiffs.subset(set1, set2).forall(_.isSuccess)  must_== false
    def e6 = FormDiffs.subset(set1, set1 ++ set2).exists(_.isSuccess) &&
             FormDiffs.subset(set1, set1 ++ set2).exists(!_.isSuccess) must beTrue
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
    
    def e1 = sameExecution(FormDiffs.subsequence(ab ++ cd, ab), ok(ab ++ cd))
    def e2 = sameExecution(FormDiffs.subsequence(bac ++ d, abc), ko(b) ++ ok(a) ++ ko(c) ++ ok(d))
    def e3 = sameExecution(FormDiffs.subsequence(cd, ab), cd ++ ko(ab))
                         
    def e4 = sameExecution(FormDiffs.subsequence(ab, ab ++ cd), ok(ab) ++ ko(cd))
    def e5 = sameExecution(FormDiffs.subsequence(ab, ba ++ cd), ko(a) ++ ok(b) ++ ko(cd))
  }

  object set {
    val set1 = List(Form.tr("a"), Form.tr("b"))
    val set2 = List(Form.tr("c"), Form.tr("d"))
    
    def e1 = sameExecution(FormDiffs.set(set1, set1 ++ set2), ok(set1) ++ ko(set2))
    def e2 = sameExecution(FormDiffs.set(set1 ++ set2, set2), ko(set1) ++ ok(set2))
    def e3 = sameExecution(FormDiffs.set(set1, set2), ko(set1 ++ set2))
    def e4 = sameExecution(FormDiffs.set(set1, set1), ok(set1))
  }
  
  object sequence {
    
    def e1 = sameExecution(FormDiffs.sequence(ab, ab ++ cd), ok(ab) ++ ko(cd))
    def e2 = sameExecution(FormDiffs.sequence(ab, ba), ko(a) ++ ok(b))
    def e3 = sameExecution(FormDiffs.sequence(ab, ba ++ c), ko(a) ++ ok(b) ++ ko(c))
    def e4 = sameExecution(FormDiffs.sequence(abc, ba), ko(a) ++ ok(b) ++ ko(c))
  }

  def ok(f: Seq[Form]) = f.map(_.setSuccess)
  def ko(f: Seq[Form]) = f.map(_.setFailure)
}