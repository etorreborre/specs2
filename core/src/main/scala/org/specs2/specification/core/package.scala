package org.specs2.specification

package object core {
  /** this allows the creation of fragments with a kind of for loop: (1 to 10).repeat(i => "ex"+i ! ok  */
  implicit class foreachRange(range: Range) {
    def repeat(f: Int => Fragments): Fragments =
      range.foldLeft(Fragments()) { (res, cur) => res.append(f(cur)) }
  }

}
