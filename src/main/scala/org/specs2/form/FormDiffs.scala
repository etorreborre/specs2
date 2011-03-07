package org.specs2
package form

/**
 * This trait provides methods to perform differences on sequences of Forms.
 *
 * @see org.specs2.guide.Forms for specification and usages
 */
private[specs2]
trait FormDiffs {

  def subset(form1: Seq[Form], form2: Seq[Form]): Seq[Form] = {
    val intersection = form1.toSet intersect form2.toSet
    form1.map { f =>
      if  (intersection contains f)  f.setSuccess
      else f
    } ++
    form2.collect { case f if !(intersection contains f) => f.setFailure }
  }

  def subsequence(form1: Seq[Form], form2: Seq[Form]): Seq[Form] = {
    val (start, rest) = form1 span (Some(_) != form2.headOption)

    start.collect {
      case f if form2 contains f => f.setFailure
      case f                     => f 
    } ++
    (rest zip form2).map { cur => cur match {
        case (f1, f2) if (f1 == f2)           => f2.setSuccess
        case (f1, f2) if (form2 contains f1)  => f1.setFailure
        case (f1, f2)                         => f1
      }
    } ++
    (rest drop form2.size) ++
    (form2.toSet diff form1.toSet).map(_.setFailure)
  }

  def set(form1: Seq[Form], form2: Seq[Form]): Seq[Form] = {
    val intersection = form1.toSet intersect form2.toSet
    form1.collect { case f =>
      if  (intersection contains f)  f.setSuccess
      else f.setFailure
    } ++
    form2.collect { case f if !(intersection contains f) => f.setFailure }
  }

  def sequence(form1: Seq[Form], form2: Seq[Form]): Seq[Form] = {
    val (start, rest) = form1 span (Some(_) != form2.headOption)

    start.map (_.setFailure) ++
    (rest zip form2).map { cur => cur match {
        case (f1, f2) if (f1 == f2)           => f2.setSuccess
        case (f1, f2)                         => f1.setFailure
      }
    } ++
    (rest drop form2.size).map(_.setFailure) ++
    (form2.toSet diff form1.toSet).map(_.setFailure)
  }
}
object FormDiffs extends FormDiffs