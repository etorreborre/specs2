package org.specs2
package specification

import control.Exceptions._
import execute._
import util.matching.Regex
import control.ImplicitParameters

/**
 * A Regular expression step which takes a text and extracts meaningful values according to
 * a regular expression:
 *
 * - either a user-defined expression for the *full text to extract*: e.g. "Given the following number: (.*)"
 * - or a default regex for groups capturing value delimited with `${}`
 *
 * It provides methods to extract either all the groups as a list, or a number of values as a tuple
 *
 */
abstract class RegexStep[P, T](fullRegex: String = "", groupRegex: String = RegexStep.DEFAULT_REGEX) {
  private lazy val full: Regex = fullRegex.r
  private lazy val group: Regex = groupRegex.r
  /** regex to use for a step with a partial function */
  protected lazy val regexToUse = if (full.toString.isEmpty) group else full

  /** remove value markers `${}` from the text */
  def strip(text: String): String = RegexStep.strip(text, full, group)

  /** remove value markers `${}` from a Text fragment */
  def strip(f: Fragment): Fragment =
    f match {
      case Text(t) => Text(strip(t))
      case other   => other
    }

  /** extract the value contained in the first group */
  def extract1(t: String) = RegexStep.extract1(t, full, group)
  def extract2(t: String) = RegexStep.extract2(t, full, group)
  def extract3(t: String) = RegexStep.extract3(t, full, group)
  def extract4(t: String) = RegexStep.extract4(t, full, group)
  def extract5(t: String) = RegexStep.extract5(t, full, group)
  def extract6(t: String) = RegexStep.extract6(t, full, group)
  def extract7(t: String) = RegexStep.extract7(t, full, group)
  def extract8(t: String) = RegexStep.extract8(t, full, group)
  def extract9(t: String) = RegexStep.extract9(t, full, group)
  def extract10(t: String)= RegexStep.extract10(t, full, group)
  def extractAll(t: String) = RegexStep.extractAll(t, full, group)
}

object RegexStep {
  val DEFAULT_REGEX = """\$\{([^}]+)\}"""

  def extract[R](text: String, f: PartialFunction[Any, R], regexToUse: Regex = DEFAULT_REGEX.r): R =
    regexToUse.findAllIn(text).size match {
      case 1 => f(extract1(text))
      case 2 => f(extract2(text))
      case 3 => f(extract3(text))
      case 4 => f(extract4(text))
      case 5 => f(extract5(text))
      case 6 => f(extract6(text))
      case 7 => f(extract7(text))
      case 8 => f(extract8(text))
      case 9 => f(extract9(text))
      case 10 => f(extract10(text))
    }

  /** extract all groups and return a list of strings */
  def extractAll(text: String, full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) =
    if (full.toString.isEmpty) group.findAllIn(text).matchData.collect { case Regex.Groups(g) => g }.toList
    else                       full.unapplySeq(text).get

  def strip(text: String): String = strip(text, "".r, DEFAULT_REGEX.r)
  /**
   * Apparently, the expression to replace can have any regex special character except '\'
   */
  def strip(text: String, full: Regex, group: Regex): String =
    if (full.toString.isEmpty) group.replaceAllIn(text, (_:Regex.Match) match { case Regex.Groups(v) => v.replace("\\", "\\\\") })
    else                       text

  def extract1(t: String , full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(1, t, (extractAll(t, full, group): @unchecked) match { case s1::_ => s1 }                                                                   )
  def extract2(t: String , full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(2, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::_ => (s1,s2) }                                                          )
  def extract3(t: String , full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(3, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::_ => (s1,s2,s3) }                                                   )
  def extract4(t: String , full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(4, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::_ => (s1,s2,s3,s4) }                                            )
  def extract5(t: String , full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(5, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::_ => (s1,s2,s3,s4,s5) }                                     )
  def extract6(t: String , full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(6, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::_ => (s1,s2,s3,s4,s5,s6) }                              )
  def extract7(t: String , full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(7, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::_ => (s1,s2,s3,s4,s5,s6,s7) }                       )
  def extract8(t: String , full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(8, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::s8::_ => (s1,s2,s3,s4,s5,s6,s7,s8) }                )
  def extract9(t: String , full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(9, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::s8::s9::_ => (s1,s2,s3,s4,s5,s6,s7,s8,s9) }         )
  def extract10(t: String, full: Regex = "".r, group: Regex = DEFAULT_REGEX.r) = check(10, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::s8::s9::s10::_ => (s1,s2,s3,s4,s5,s6,s7,s8,s9,s10) })

  private def check[T](variablesNb: Int, string: String, extract: =>T) = tryOr(extract) {
    case e: MatchError => if (variablesNb == 1) string else throw new FailureException(Failure("couldn't extract "+variablesNb+" variables from: "+string))
    case other         => throw other
  }
}

/**
 * This step can start a sequence of Given / When / Then.
 *
 * It must define the extract function creating a value of type T from the extracted values
 */
abstract class Given[T](val regex: String = "", val groupRegex: String = RegexStep.DEFAULT_REGEX) extends RegexStep[Unit, T](regex, groupRegex) {
  /** if the extraction goes wrong, then an Error is propagated */
  private[specs2] def extractContext(text: String): Either[Result, T] = ResultExecution.executeEither(extract(text))(identity)

  def extract(text: String): T
}

/** implicit conversions to create Given objects */
object Given extends ImplicitParameters {
  implicit def function1ToGiven[T](f: String => T): Given[T] = new Given[T] { def extract(text: String) = f(extract1(text))  }
  implicit def function2ToGiven[T](f: (String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract2(text))  }
  implicit def function3ToGiven[T](f: (String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract3(text))  }
  implicit def function4ToGiven[T](f: (String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract4(text))  }
  implicit def function5ToGiven[T](f: (String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract5(text))  }
  implicit def function6ToGiven[T](f: (String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract6(text))  }
  implicit def function7ToGiven[T](f: (String, String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract7(text))  }
  implicit def function8ToGiven[T](f: (String, String, String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract8(text))  }
  implicit def function9ToGiven[T](f: (String, String, String, String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract9(text))  }
  implicit def function10ToGiven[T](f:(String, String, String, String, String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract10(text))  }
  implicit def functionSeqToGiven[T](f: Seq[String] => T)(implicit p: ImplicitParam): Given[T] = new Given[T] { def extract(text: String) = f(extractAll(text))  }
}

/**
 * This step define conditions in a sequence of Given / When / Then.
 *
 * It must define the extract function taking the previous state of extracted values, P, and creating a new state
 * of type T from the extracted values
 */
abstract class When[P, T](val regex: String = "", val groupRegex: String = RegexStep.DEFAULT_REGEX) extends RegexStep[P, T](regex, groupRegex) {
  /**
   * if the previous extraction went wrong, then a Skipped result is propagated.
   * Otherwise if the current extraction goes wrong, then an Error is propagated
   */
  private[specs2] def extractContext(p: Either[Result, P], text: String): Either[Result, T] = p match {
    case Left(l)  => Left(Skipped(l.message))
    case Right(r) => trye(extract(r, text))((e:Exception) => Error(e))
  }
  def extract(p: P, text: String): T
}
/** implicit conversions to create When objects */
object When extends ImplicitParameters {
  implicit def function1ToWhen[T, S](f: T => String => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t)(extract1(text))  }
  implicit def function2ToWhen[T, S](f: T => (String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract2(text))  }
  implicit def function3ToWhen[T, S](f: T => (String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract3(text))  }
  implicit def function4ToWhen[T, S](f: T => (String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract4(text))  }
  implicit def function5ToWhen[T, S](f: T => (String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract5(text)) }
  implicit def function6ToWhen[T, S](f: T => (String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract6(text)) }
  implicit def function7ToWhen[T, S](f: T => (String, String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract7(text)) }
  implicit def function8ToWhen[T, S](f: T => (String, String, String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract8(text)) }
  implicit def function9ToWhen[T, S](f: T => (String, String, String, String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract9(text)) }
  implicit def function10ToWhen[T, S](f: T => (String, String, String, String, String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract10(text)) }
  implicit def functionSeqToWhen[T, S](f: T => Seq[String] => S)(implicit p: ImplicitParam): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t)(extractAll(text))  }
}

/**
 * This step define checks in a sequence of Given / When / Then.
 *
 * It must define the extract function taking the state of extracted values, T, and return a `Result`
 */
abstract class Then[T](regex: String = "", val groupRegex: String = RegexStep.DEFAULT_REGEX) extends RegexStep[Either[Result, T], (T, Result)](regex, groupRegex) {
  /**
   * if the previous extraction went wrong, then a Skipped result is propagated.
   * Otherwise if the current extraction goes wrong, then an Error is propagated
   */
  private[specs2] def extractContext(t: Either[Result, T], text: String): Either[Result, (T, Result)] = t match {
    case Left(l)  => Left(Skipped(l.message))
    case Right(r) => trye((r, extract(r, text)))((e:Exception) => Error(e))
  }
  def extract(t: T, text: String): Result
}
/** implicit conversions to create Then objects */
object Then extends ImplicitParameters {
  implicit def function1ToThen[T, R : AsResult](f: T => String => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t)(extract1(text)))  }
  implicit def function2ToThen[T, R : AsResult](f: T => (String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract2(text)))  }
  implicit def function3ToThen[T, R : AsResult](f: T => (String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract3(text)))  }
  implicit def function4ToThen[T, R : AsResult](f: T => (String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract4(text)))  }
  implicit def function5ToThen[T, R : AsResult](f: T => (String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract5(text)))  }
  implicit def function6ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract6(text)))  }
  implicit def function7ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract7(text)))  }
  implicit def function8ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract8(text)))  }
  implicit def function9ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract9(text)))  }

  implicit def function10ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract10(text)))  }
  implicit def functionSeqToThen[T, R](f: T => Seq[String] => R)(implicit r: AsResult[R], p: ImplicitParam): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t)(extractAll(text)))  }
}

abstract class GivenThen(regex: String= "") extends RegexStep[String, Result](regex) {
  def extract(text: String): Result
}

class GivenThenFunction[R : AsResult](f: PartialFunction[Any, R], regex: String= "") extends GivenThen(regex) {
  def extract(text: String): Result = RegexStep.extract(text, { case a => AsResult(f(a)) }, regexToUse)
}
object so {
  def apply[R : AsResult](f: PartialFunction[Any, R]): GivenThen = new GivenThenFunction(f)
}
