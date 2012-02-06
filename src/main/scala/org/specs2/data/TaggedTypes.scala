package org.specs2
package data

trait TaggedTypes {
  // Unboxed newtypes, credit to @milessabin and @retronym
  type Tagged[U] = { type Tag = U }
  type @@[T, U] = T with Tagged[U]

  class Tagger[U] {  def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U] }
  def tag[U] = new Tagger[U]
}