package org.specs2
package form

import FormsExamples._

class FormsSpec extends Spec with FormsBuilder { def is = s2"""

The Forms object provides several utility functions for creating forms

  # The subset method allows to check if a list of forms is a subset of another one
    subset(l1 + l2, l1) == l1 + l2 - ok                       $subset1
    subset(l1, l2) == l1 - ko                                 $subset2
    subset(l1 + l2, l1) == l1 - ok + l2 - ko                  $subset3

  # If the subset method fails, the form are shown as failed
    subset(l1, l1 + l2) == l1 - ok                            $subset4
    subset(l1, l2) == l1 - ko                                 $subset5
    subset(l1 + l2, l1) == l1 - ok + l2 - ko                  $subset6

  # The subsequence method allows to check if a list of forms is a subsequence
  of another one
    subsequence(ab + cd, ab) == abcd - ok                          $subsequence1
    subsequence(bac + d, abc) == b - ko + a - ok + c - ko + d - ok $subsequence2
    subsequence(cd, ab) == cd - ok + ab - ko                       $subsequence3
    subsequence(ab, ab + cd) == ab - ok + cd - ko                  $subsequence4
    subsequence(ab, ba + cd) == ax - ko + cd - ko                  $subsequence5

  # The set method allows to check if 2 lists of forms are the same, in no specific order
    set(l1, l1 + l2) == l1 - ok + l2 ko $checkSet1
    set(l1 + l2, l2) == l1 - ko + l2 ok $checkSet2
    set(l1, l2) == l1 - ko              $checkSet3
    set(l1, l1) == l1 - ok              $checkSet4

  # The sequence method allows to check if 2 lists of forms are the same, in order
    sequence(ab, ab + cd) == ab - ok + cd - ko $checkSequence1
    sequence(ab, ba) == ab - ko                $checkSequence2
    sequence(ab, ba + c) == a - ok + bc - ko   $checkSequence3
    sequence(abc, ba) == a - ok + bc - ko      $checkSequence4

"""

  val set1 = List(Form.tr("a"), Form.tr("b"))
  val set2 = List(Form.tr("c"), Form.tr("d"))

  def subset1 = FormDiffs.subset(set1 ++ set2, set2) must_== set1 ++ ok(set2)
  def subset2 = FormDiffs.subset(set1, set2) must_== set1 ++ ko(set2)
  def subset3 = FormDiffs.subset(set1, set1 ++ set2) must_== ok(set1) ++ ko(set2)
  def subset4 = FormDiffs.subset(set1 ++ set2, set1).forall(_.isSuccess) must beTrue
  def subset5 = FormDiffs.subset(set1, set2).forall(_.isSuccess)  must_== false
  def subset6 = FormDiffs.subset(set1, set1 ++ set2).exists(_.isSuccess) &&
    FormDiffs.subset(set1, set1 ++ set2).exists(!_.isSuccess) must beTrue

  def subsequence1 = sameExecution(FormDiffs.subsequence(ab ++ cd, ab), ok(ab ++ cd))
  def subsequence2 = sameExecution(FormDiffs.subsequence(bac ++ d, abc), ko(b) ++ ok(a) ++ ko(c) ++ ok(d))
  def subsequence3 = sameExecution(FormDiffs.subsequence(cd, ab), cd ++ ko(ab))
  def subsequence4 = sameExecution(FormDiffs.subsequence(ab, ab ++ cd), ok(ab) ++ ko(cd))
  def subsequence5 = sameExecution(FormDiffs.subsequence(ab, ba ++ cd), ko(a) ++ ok(b) ++ ko(cd))

  def checkSet1 = sameExecution(FormDiffs.set(set1, set1 ++ set2), ok(set1) ++ ko(set2))
  def checkSet2 = sameExecution(FormDiffs.set(set1 ++ set2, set2), ko(set1) ++ ok(set2))
  def checkSet3 = sameExecution(FormDiffs.set(set1, set2), ko(set1 ++ set2))
  def checkSet4 = sameExecution(FormDiffs.set(set1, set1), ok(set1))

  def checkSequence1 = sameExecution(FormDiffs.sequence(ab, ab ++ cd), ok(ab) ++ ko(cd))
  def checkSequence2 = sameExecution(FormDiffs.sequence(ab, ba), ko(a) ++ ok(b))
  def checkSequence3 = sameExecution(FormDiffs.sequence(ab, ba ++ c), ko(a) ++ ok(b) ++ ko(c))
  def checkSequence4 = sameExecution(FormDiffs.sequence(abc, ba), ko(a) ++ ok(b) ++ ko(c))

  def sameExecution(f1: Seq[Form], f2: Seq[Form]) = f1.map(_.execute.message) must_== f2.map(_.execute.message)

  def ok(f: Seq[Form]) = f.map(_.setSuccess)
  def ko(f: Seq[Form]) = f.map(_.setFailure)
}

object FormsExamples extends FormsBuilder:
  val (a, b, c, d)     = (List(Form.tr("a")), List(Form.tr("b")), List(Form.tr("c")), List(Form.tr("d")))
  val (ab, ba, bc, cd) = (List(Form.tr("a"), Form.tr("b")), List(Form.tr("b"), Form.tr("a")), List(Form.tr("b"), Form.tr("c")), List(Form.tr("c"), Form.tr("d")))
  val (abc, bac)       = (List(Form.tr("a"), Form.tr("b"), Form.tr("c")), List(Form.tr("b"), Form.tr("a"), Form.tr("c")))
