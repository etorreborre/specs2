package org.specs2
package matcher

import execute.*
import Result.ResultFailureMonoid
import text.Quote.*
import text.Plural.*
import org.specs2.fp.syntax.*
import control.*
import ValueChecks.{given}

/** This trait adds some implicits to easily fold sequences of results
  */
trait MatcherImplicits extends ExpectationsCreation:

  /** Extend collections to check all their elements
    */
  extension [T, R: AsResult](values: Traversable[T])
    def atLeastOnce(f: T => R): Result =
      createExpectable(values).applyMatcher(ContainWithResult(f).atLeastOnce)

  def atMostOnce(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).atMostOnce)

  def atLeast(n: Times)(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).atLeast(n))

  def atLeast(n: Int)(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).atLeast(n))

  def atMost(n: Times)(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).atMost(n))

  def atMost(n: Int)(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).atMost(n))

  def between(min: Times, max: Times)(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).between(min, max))

  def between(min: Int, max: Int)(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).between(min, max))

  def exactly(n: Times)(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).exactly(n))

  def exactly(n: Int)(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).exactly(n))

  /** verify the function f for all the values, stopping after the first failure, where the PartialFunction is defined
    */
  def forall[T, R: AsResult](values: Traversable[T])(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).forall)

  def forallWhen[T, R: AsResult](values: Traversable[T])(f: PartialFunction[T, R]): Result =
    forall(values.filter(f.isDefinedAt))(f)

  /** verify the function f for all the values, and collect all failures */
  def foreach[T, R: AsResult](values: Traversable[T])(f: T => R): Result =
    createExpectable(values).applyMatcher(ContainWithResult(f).foreach)

  /** verify the function f for all the values, and collect all failures, where the PartialFunction is defined */
  def foreachWhen[T, R: AsResult](values: Traversable[T])(f: PartialFunction[T, R]): Result =
    foreach(values.filter(f.isDefinedAt))(f)

  /** verify the function f for at least one value, where the PartialFunction is defined */
  def atLeastOnceWhen[T, R: AsResult](values: Traversable[T])(f: PartialFunction[T, R]): Result =
    atLeastOnce(values.filter(f.isDefinedAt))(f)

  /** verify the function f for at least one value, where the PartialFunction is defined */
  def atMostOnceWhen[T, R: AsResult](values: Traversable[T])(f: PartialFunction[T, R]): Result =
    atMostOnce(values.filter(f.isDefinedAt))(f)

  /** this extension provides an inverted syntax to adapt matchers to make the adaptation more readable in some cases:
    *   - def haveExtension(extension: =>String) = ((_:File).getPath) ^^ endWith(extension)
    */
  extension [T, S](f: T => S)
    def ^^(m: Matcher[S]): Matcher[T] =
      m ^^ f

object MatcherImplicits extends MatcherImplicits
