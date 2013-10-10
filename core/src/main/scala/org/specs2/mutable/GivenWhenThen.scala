package org.specs2
package mutable

import specification.{Given, When, Then}
import text.RegexExtractor
import RegexExtractor._
import execute.AsResult
import specification.{FormattingFragments => FF, _}

trait GivenWhenThen extends org.specs2.specification.GivenWhenThen { outer: FragmentsBuilder =>

  /**
   * add a new example using 'in' or '>>' or '!'
   */
  implicit def givenThenInExample(s: String): GivenThenInExample = new GivenThenInExample(s)
  /** transient class to hold an example description before creating a full Example */
  class GivenThenInExample(s: String) {
    def in(gt: GivenThen): Example = exampleFactory.newExample(Example(RegexExtractor.strip(s), gt.extract(s)))
    def >>(gt: GivenThen): Example = exampleFactory.newExample(Example(RegexExtractor.strip(s), gt.extract(s)))
  }

  /**
   * Create GWT fragments with the << syntax for a mutable specification
   */
  implicit def gwtToFragment(s: String): GWTToFragment = new GWTToFragment(s)
  class GWTToFragment(s: String) {
    def <<(given: Given[Unit]): Fragments = createStep(s, given.extract(s))
    def <<(u: =>Unit): Step = createStep(s, u)
    def <<(f: Function[String, Unit]): Fragments = createStep(s, f(extract1(s)))
    def <<(f: Function2[String, String, Unit]): Fragments = createStep(s, f.tupled(extract2(s)))
    def <<(f: Function3[String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract3(s)))
    def <<(f: Function4[String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract4(s)))
    def <<(f: Function5[String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract5(s)))
    def <<(f: Function6[String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract6(s)))
    def <<(f: Function7[String, String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract7(s)))
    def <<(f: Function8[String, String, String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract8(s)))
    def <<(f: Function9[String, String, String, String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract9(s)))
    def <<(f: Function10[String, String, String, String, String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract10(s)))
    def <<(f: Seq[String] => Unit)(implicit p: ImplicitParam): Fragments = createStep(s, f(extractAll(s)))

    def <<(andThen: Then[Unit]): Example = outer.createExample(s, andThen.extract((), s))
    def <<[R : AsResult](r: =>R): Example = createExample(s, r)
    def <<[R : AsResult](f: Function[String, R]): Example = createExample(s, f(extract1(s)))
    def <<[R : AsResult](f: Function2[String, String, R]): Example = createExample(s, f.tupled(extract2(s)))
    def <<[R : AsResult](f: Function3[String, String, String, R]): Example = createExample(s, f.tupled(extract3(s)))
    def <<[R : AsResult](f: Function4[String, String, String, String, R]): Example = createExample(s, f.tupled(extract4(s)))
    def <<[R : AsResult](f: Function5[String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract5(s)))
    def <<[R : AsResult](f: Function6[String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract6(s)))
    def <<[R : AsResult](f: Function7[String, String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract7(s)))
    def <<[R : AsResult](f: Function8[String, String, String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract8(s)))
    def <<[R : AsResult](f: Function9[String, String, String, String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract9(s)))
    def <<[R : AsResult](f: Function10[String, String, String, String, String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract10(s)))
    def <<[R](f: Seq[String] => R)(implicit r: AsResult[R], p: ImplicitParam): Example = createExample(s, f(extractAll(s)))

  }

  private def createStep(s: String, u: =>Unit) = {
    strip(s).txt
    addFragments(FF.bt)
    step(u)
  }

  private def createExample[R : AsResult](s: String, r: =>R) = {
    addFragments(FF.t)
    val e = exampleFactory.newExample(strip(s), r)
    addFragments(FF.bt)
    e
  }

}
