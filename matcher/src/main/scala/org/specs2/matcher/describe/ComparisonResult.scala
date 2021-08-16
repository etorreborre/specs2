package org.specs2.matcher.describe

import java.io.{PrintWriter, StringWriter}

import org.specs2.matcher.describe.ComparisonResultOps.*
import org.specs2.text.Quote.*
import org.specs2.text.NotNullStrings.*
import org.specs2.text.Indent.*

/** Render the result of a comparison for different types: primitives, throwables, collections,...
  *
  * The comparison may turn out to render identical values or differences
  */
trait ComparisonResult:
  def identical: Boolean
  def render: String
  def render(indent: String): String = render

trait IdenticalComparisonResult extends ComparisonResult:
  val identical = true

trait DifferentComparisonResult extends ComparisonResult:
  val identical = false

case class PrimitiveIdentical(value: Any) extends IdenticalComparisonResult:
  def render: String =
    value.render

case class ThrowableIdentical(value: Throwable) extends IdenticalComparisonResult:
  def render: String =
    val w = new StringWriter
    value.printStackTrace(new PrintWriter(w))
    w.toString

case class ThrowableDifferentMessage(result: ComparisonResult) extends DifferentComparisonResult:
  def render: String =
    "\nthe message is incorrect\n" +
      result.render

case class ThrowableDifferentStackTrace(result: ComparisonResult) extends DifferentComparisonResult:
  def render: String =
    "\nthe stacktrace is incorrect\n" +
      result.render

case class StackElementIdentical(value: StackTraceElement) extends IdenticalComparisonResult:
  def render: String =
    value.toString

case class StackElementDifferent(
    className: ComparisonResult,
    methodName: ComparisonResult,
    fileName: Option[ComparisonResult],
    lineNumber: ComparisonResult
) extends DifferentComparisonResult:
  def render: String =
    s"${className.render}.${methodName.render}$renderSourceLocation"

  private def renderSourceLocation: String =
    fileName
      .map(f => s"(${f.render}:${lineNumber.render})")
      .getOrElse("(Unknown Source)")

case class PrimitiveDifference(actual: Any, expected: Any) extends DifferentComparisonResult:
  def render: String =
    (actual, expected).renderDiff

case class SetIdentical(value: Set[?]) extends OrderedCollectionIdentical(value) with SetTypeProvider

case class SetDifference(same: Seq[Any], added: Seq[Any], removed: Seq[Any])
    extends UnorderedCollectionDifferent(same, Seq.empty[Any], added, removed)
    with SetTypeProvider:

  protected def renderElement(indent: String)(element: Any): String =
    element.render

  protected def renderChange(indent: String)(change: Any): String =
    change.render

trait SetTypeProvider:
  val className = "Set"

case class SeqIdentical(value: Seq[Any]) extends OrderedCollectionIdentical(value) with ListTypeProvider

case class SeqDifference(result: Seq[ComparisonResult], added: Seq[Any], removed: Seq[Any])
    extends OrderedCollectionDifferent(result, added, removed)
    with ListTypeProvider

trait ListTypeProvider:
  val className = "List"

case class ArrayIdentical(value: Seq[Any]) extends OrderedCollectionIdentical(value) with ArrayTypeProvider

case class ArrayDifference(results: Seq[ComparisonResult], added: Seq[Any], removed: Seq[Any])
    extends OrderedCollectionDifferent(results, added, removed)
    with ArrayTypeProvider

trait ArrayTypeProvider:
  val className = "Array"

case class MapIdentical(m: Map[?, ?]) extends OrderedCollectionIdentical(m) with MapTypeProvider

trait MapTypeProvider:
  val className = "Map"

case class MapDifference(
    same: Seq[(Any, Any)],
    changed: Seq[(Any, ComparisonResult)],
    added: Seq[(Any, Any)],
    removed: Seq[(Any, Any)]
) extends UnorderedCollectionDifferent(same, changed, added, removed)
    with MapTypeProvider:

  protected def renderElement(indent: String)(element: (Any, Any)): String =
    element.render

  protected def renderChange(indent: String)(change: (Any, ComparisonResult)): String =
    s"${change._1.render} -> {${change._2.render}}"

case class OptionIdentical(same: Option[ComparisonResult]) extends IdenticalComparisonResult:
  def render: String =
    same.map(_.render.wrapWith("Some")).getOrElse("None")

case class EitherIdentical(same: ComparisonResult, isRight: Boolean) extends IdenticalComparisonResult:
  def render: String =
    same.render.wrapWith(if isRight then "Right" else "Left")

case class TryIdentical(same: Any, isSuccess: Boolean) extends IdenticalComparisonResult:
  def render: String =
    same.render.wrapWith(if isSuccess then "Success" else "Failure")

case class TryDifferent(value: ComparisonResult, isSuccess: Boolean) extends DifferentComparisonResult:
  def render: String =
    value.render.wrapWith(if isSuccess then "Success" else "Failure")

case class TryTypeDifferent(isActualSuccess: Boolean) extends DifferentComparisonResult:
  def render: String =
    s"${render(isActualSuccess)} ==> ${render(!isActualSuccess)}"

  private def render(success: Boolean): String =
    "...".wrapWith(if success then "Success" else "Failure")

case class EitherDifferent(changed: ComparisonResult, isRight: Boolean) extends DifferentComparisonResult:
  def render: String =
    changed.render.wrapWith(if isRight then "Right" else "Left")

case class EitherTypeDifferent(isActualRight: Boolean) extends DifferentComparisonResult:
  def render: String =
    s"${render(isActualRight)} ==> ${render(!isActualRight)}"

  private def render(right: Boolean): String =
    "...".wrapWith(if right then "Right" else "Left")

case class OptionDifferent(changed: ComparisonResult) extends DifferentComparisonResult:
  def render: String =
    changed.render.wrapWith("Some")

case class OptionTypeDifferent(isActualSome: Boolean, isExpectedSome: Boolean) extends DifferentComparisonResult:
  def render: String =
    s"${render(isActualSome)} ==> ${render(isExpectedSome)}"

  private def render(some: Boolean) = if some then "Some(...)" else "None"

case class OtherIdentical(actual: Any, expected: Any) extends IdenticalComparisonResult:
  def render: String =
    s"${actual.renderAny(showAll = comparingPrimitiveWithObject(actual, expected))} == " +
      s"${expected.renderAny(showAll = comparingPrimitiveWithObject(actual, expected))}"

case class OtherDifferent(actual: Any, expected: Any) extends DifferentComparisonResult:
  def render: String =
    val actualRendered = s"${actual.renderAny(showAll = comparingPrimitiveWithObject(actual, expected))}"
    val expectedRendered = s"${expected.renderAny(showAll = comparingPrimitiveWithObject(actual, expected))}"
    if actualRendered.contains("\n") || actualRendered.contains("\n") then s"${actualRendered}\n !=\n$expectedRendered"
    else s"$actualRendered != $expectedRendered"

private[specs2] def comparingPrimitiveWithObject(a: Any, e: Any): Boolean =
  val (classA, classB) = classOf(a) -> classOf(e)
  classA != classB && (isPrimitive(classA) ^ isPrimitive(classB))

private[specs2] def isPrimitive(clazz: String): Boolean =
  clazz.startsWith("java.lang.")

private[specs2] def classOf(v: Any): String =
  Option(v)
    .map(_.getClass.getName)
    .getOrElse("null")

abstract class OrderedCollectionIdentical(value: Iterable[Any]) extends IdenticalComparisonResult:
  def render: String =
    value.map(_.render).mkString(", ").wrapWith(className)

  def className: String

abstract class UnorderedCollectionDifferent[Element, Change](
    same: Seq[Element],
    changed: Seq[Change],
    added: Seq[Element],
    removed: Seq[Element]
) extends DifferentComparisonResult:

  override def render: String =
    render("")

  override def render(indent: String): String =
    val newIndent = indent + " " * (className.length + 1)

    Seq(
      renderIdentical(newIndent) ++ renderChanged(newIndent) ++ renderAdded(newIndent) ++ renderRemoved(newIndent)
    ).flatten.mkString("", ",\n" + newIndent, "").wrapWith(className)

  private def renderIdentical(indent: String): Option[String] =
    same.toOption.map(_.map(renderElement(indent)).mkString("", ",\n" + indent, ""))

  private def renderChanged(indent: String): Option[String] =
    changed.toOption.map(_.map(renderChange(indent)).mkString("", ",\n" + indent, ""))

  private def renderAdded(indent: String): Option[String] =
    added.toOption.map(_.map(renderElement(indent)).mkString("", ",\n" + indent, "").tagWith("added"))

  private def renderRemoved(indent: String): Option[String] =
    removed.toOption.map(_.map(renderElement(indent)).mkString("", ",\n" + indent, "").tagWith("removed"))

  def className: String

  protected def renderElement(indent: String)(element: Element): String
  protected def renderChange(indent: String)(change: Change): String

abstract class OrderedCollectionDifferent[Element](
    results: Seq[ComparisonResult],
    added: Seq[Element],
    removed: Seq[Element]
) extends DifferentComparisonResult:
  override def render: String =
    render("")

  override def render(indent: String): String =
    val newIndent = indent + " " * (className.length + 1)

    Seq(renderResult(newIndent) ++ renderAdded(newIndent) ++ renderRemoved(newIndent)).flatten
      .mkString("", ",\n" + newIndent, "")
      .wrapWith(className)

  private def renderResult(indent: String): Option[String] =
    results.toOption.map(_.map(_.render(indent)).mkString(", "))

  private def renderAdded(indent: String): Option[String] =
    added.toOption.map(_.map(_.render).mkString(", ").tagWith("added"))

  private def renderRemoved(indent: String): Option[String] =
    removed.toOption.map(_.map(_.render).mkString(", ").tagWith("removed"))

  def className: String

object ComparisonResultOps:

  case class PropertyDifference[A, E](actual: A, expected: E)

  extension (value: Any)
    def renderAny(showAll: Boolean = false): String =
      value.asInstanceOf[Matchable] match
        case v if showAll => v.notNullWithClass(showAll = true)
        case (k, v)       => s"${k.render} -> ${v.render}"
        case x: String    => q(x)
        case x            => unq(x)

    def render: String = renderAny(false)

  extension (diff: (Any, Any))
    def renderDiff: String =
      s"${diff._1.render} != ${diff._2.render}"

  extension (values: String)
    def wrapWith(`type`: String): String =
      s"${`type`}($values)"

    def tagWith(tag: String): String =
      s"$tag: $values"

  extension [T](s: Seq[T])
    def toOption: Option[Seq[T]] =
      if s.isEmpty then None else Some(s)

class ProductComparisonResult(typeName: String, results: List[(String, ComparisonResult)]) extends ComparisonResult:
  private val typeNameIndent = " " * (typeName.length + 1)

  def identical: Boolean =
    results.forall(_._2.identical)

  override def render(indent: String): String =
    typeName + "(" + results
      .map { case (fieldName, fieldResult) =>
        val fieldResultRendered = fieldResult.render
        val indented =
          if fieldResultRendered.contains("\n") then "\n" + fieldResultRendered.indentWith(indent + " " * 2)
          else " "+fieldResultRendered
        s"$fieldName:$indented"
      }
      .mkString("\n" + indent) + ")"

  def render: String =
    render(typeNameIndent)
