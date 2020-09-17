package org.specs2
package control

/**
 * This trait is used to abuse method overloading and allow some of the methods in specs2 DSL to
 * be applicable to various parameters.
 *
 * For example in a mutable specification, the >> method is overloaded for lots of different arguments:
 *
 *  - result
 *  - function of the Environment
 *  - function of the Command line
 *  - ...
 */
trait ImplicitParameters:

  trait ImplicitParam
  given implicitParameter as ImplicitParam = new ImplicitParam {}

  trait ImplicitParam1
  given implicitParameter1 as ImplicitParam1 = new ImplicitParam1 {}

  trait ImplicitParam2
  given implicitParameter2 as ImplicitParam2 = new ImplicitParam2 {}

  trait ImplicitParam3
  given implicitParameter3 as ImplicitParam3 = new ImplicitParam3 {}

  trait ImplicitParam4
  given implicitParameter4 as ImplicitParam4 = new ImplicitParam4 {}

  trait ImplicitParam5
  given implicitParameter5 as ImplicitParam5 = new ImplicitParam5 {}

  trait ImplicitParam6
  given implicitParameter6 as ImplicitParam6 = new ImplicitParam6 {}

  trait ImplicitParam7
  given implicitParameter7 as ImplicitParam7 = new ImplicitParam7 {}

  trait ImplicitParam8
  given implicitParameter8 as ImplicitParam8 = new ImplicitParam8 {}

  trait ImplicitParam9
  given implicitParameter9 as ImplicitParam9 = new ImplicitParam9 {}

  trait ImplicitParam10
  given implicitParameter10 as ImplicitParam10 = new ImplicitParam10 {}

object ImplicitParameters extends ImplicitParameters:

  // to avoid unused parameters
  def use[T](a: Any)( other: T): T =
    if false then a else a
    other
