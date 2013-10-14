package org.specs2
package data


trait TaggedTypes {
  // Unboxed newtypes, credit to @milessabin and @retronym
  type Tagged[U] = { type Tag = U }
  type @@[T, U] = T with Tagged[U]

  class Tagger[U] {  def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U] }
  def tag[U] = new Tagger[U]
  
  // Newtype from the shapeless library by @milessabin
  
  /**
   * New type with `Repr` as representation type and operations provided by `Ops`.
   * 
   * Values of the newtype will not add any additional boxing beyond what's required for
   * values of the representation type to conform to Any. In practice this means that value
   * types will receive their standard Scala AnyVal boxing and reference types will be unboxed.
   */
  type Newtype[Repr, Ops] = Any @@ NewtypeTag[Repr, Ops]
  trait NewtypeTag[Repr, Ops]
  
  /**
   * Creates a value of the newtype given a value of its representation type. 
   */
  def newtype[Repr, Ops](r : Repr) : Newtype[Repr, Ops] = r.asInstanceOf[Newtype[Repr, Ops]]
  
  /**
   * Implicit conversion of newtype to `Ops` type for the selection of `Ops` newtype operations.
   * 
   * The implicit conversion `Repr => Ops` would typically be provided by publishing the companion
   * object of the `Ops` type as an implicit value.
   */
  implicit def newtypeOps[Repr, Ops](t : Newtype[Repr, Ops])(implicit mkOps : Repr => Ops) : Ops = t.asInstanceOf[Repr] 
}