package org.specs2.specification

package object core {
  /** this allows the creation of fragments with a kind of for loop: (1 to 10).repeat(i => "ex"+i ! ok)  */
  implicit class foreachInSequence[T](seq: Seq[T]) {
    def repeat(f: T => Fragments): Fragments =
      seq.foldLeft(Fragments()) { (res, cur) => res.append(f(cur)) }
  }

}
