package org.specs2.matcher.describe

trait DifferenceInspector {
  def diff[T](actual: T, expected: T)(implicit di: Diffable[T]): ComparisonResult
}

class DefaultDifferenceInspector extends DifferenceInspector {
  def diff[T](actual: T, expected: T)(implicit di: Diffable[T]): ComparisonResult =
    di.diff(actual, expected)
}
