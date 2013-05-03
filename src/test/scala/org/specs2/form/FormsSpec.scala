package org.specs2
package form

import specification._

class FormsSpec extends script.Specification with FormsBuilder with Grouped { def is = s2"""

The Forms object provides several utility functions for creating forms
                                                                                          
  The subset method allows to check if a list of forms is a subset of another one       
    + subset(l1 + l2, l1) == l1 + l2 - ok
    + subset(l1, l2) == l1 - ko
    + subset(l1 + l2, l1) == l1 - ok + l2 - ko
                                                                                          
  If the subset method fails, the form are shown as failed                              
    + subset(l1, l1 + l2) == l1 - ok
    + subset(l1, l2) == l1 - ko
    + subset(l1 + l2, l1) == l1 - ok + l2 - ko
                                                                                          
  The subsequence method allows to check if a list of forms is a subsequence            
  of another one                                                                        
    + subsequence(ab + cd, ab) == abcd - ok
    + subsequence(bac + d, abc) == b - ko + a - ok + c - ko + d - ok
    + subsequence(cd, ab) == cd - ok + ab - ko
    + subsequence(ab, ab + cd) == ab - ok + cd - ko
    + subsequence(ab, ba + cd) == ax - ko + cd - ko
                                                                                          
  The set method allows to check if 2 lists of forms are the same, in no specific order 
    + set(l1, l1 + l2) == l1 - ok + l2 ko
    + set(l1 + l2, l2) == l1 - ko + l2 ok
    + set(l1, l2) == l1 - ko
    + set(l1, l1) == l1 - ok
                                                                                          
  The sequence method allows to check if 2 lists of forms are the same, in order        
    + sequence(ab, ab + cd) == ab - ok + cd - ko
    + sequence(ab, ba) == ab - ko
    + sequence(ab, ba + c) == a - ok + bc - ko
    + sequence(abc, ba) == a - ok + bc - ko
                                                                                        """

  "subset verification" - new g1 {
    val set1 = List(Form.tr("a"), Form.tr("b"))
    val set2 = List(Form.tr("c"), Form.tr("d"))
    
    e1 := FormDiffs.subset(set1 ++ set2, set2) must_== set1 ++ ok(set2)
    e2 := FormDiffs.subset(set1, set2) must_== set1 ++ ko(set2)
    e3 := FormDiffs.subset(set1, set1 ++ set2) must_== ok(set1) ++ ko(set2)
    
    e4 := FormDiffs.subset(set1 ++ set2, set1).forall(_.isSuccess) must beTrue
    e5 := FormDiffs.subset(set1, set2).forall(_.isSuccess)  must_== false
    e6 := FormDiffs.subset(set1, set1 ++ set2).exists(_.isSuccess) &&
          FormDiffs.subset(set1, set1 ++ set2).exists(!_.isSuccess) must beTrue
  }

  "subsequence verification" - new g2 with forms {
    e1 := sameExecution(FormDiffs.subsequence(ab ++ cd, ab), ok(ab ++ cd))
    e2 := sameExecution(FormDiffs.subsequence(bac ++ d, abc), ko(b) ++ ok(a) ++ ko(c) ++ ok(d))
    e3 := sameExecution(FormDiffs.subsequence(cd, ab), cd ++ ko(ab))
                         
    e4 := sameExecution(FormDiffs.subsequence(ab, ab ++ cd), ok(ab) ++ ko(cd))
    e5 := sameExecution(FormDiffs.subsequence(ab, ba ++ cd), ko(a) ++ ok(b) ++ ko(cd))
  }

  "set verification" - new g3 {
    val set1 = List(Form.tr("a"), Form.tr("b"))
    val set2 = List(Form.tr("c"), Form.tr("d"))
    
    e1 := sameExecution(FormDiffs.set(set1, set1 ++ set2), ok(set1) ++ ko(set2))
    e2 := sameExecution(FormDiffs.set(set1 ++ set2, set2), ko(set1) ++ ok(set2))
    e3 := sameExecution(FormDiffs.set(set1, set2), ko(set1 ++ set2))
    e4 := sameExecution(FormDiffs.set(set1, set1), ok(set1))
  }
  
  "sequence verification" - new g4 with forms {
    e1 := sameExecution(FormDiffs.sequence(ab, ab ++ cd), ok(ab) ++ ko(cd))
    e2 := sameExecution(FormDiffs.sequence(ab, ba), ko(a) ++ ok(b))
    e3 := sameExecution(FormDiffs.sequence(ab, ba ++ c), ko(a) ++ ok(b) ++ ko(c))
    e4 := sameExecution(FormDiffs.sequence(abc, ba), ko(a) ++ ok(b) ++ ko(c))
  }

  trait forms {
    val (a, b, c, d)     = (List(Form.tr("a")), List(Form.tr("b")), List(Form.tr("c")), List(Form.tr("d")))
    val (ab, ba, bc, cd) = (List(Form.tr("a"), Form.tr("b")), List(Form.tr("b"), Form.tr("a")), List(Form.tr("b"), Form.tr("c")), List(Form.tr("c"), Form.tr("d")))
    val (abc, bac)       = (List(Form.tr("a"), Form.tr("b"), Form.tr("c")), List(Form.tr("b"), Form.tr("a"), Form.tr("c")))
  }

  def sameExecution(f1: Seq[Form], f2: Seq[Form]) = f1.map(_.execute.message) must_== f2.map(_.execute.message)

  def ok(f: Seq[Form]) = f.map(_.setSuccess)
  def ko(f: Seq[Form]) = f.map(_.setFailure)
}