package org.specs2.control

 import org.specs2.collection.canEqualAny
 
/**
 * Utility object to show a parameter as used
 * In particular this is useful to avoid warnings with ImplicitParameters
 */
object Use:

  def apply(t: Any, ts: Any*): Unit =
    if false && t == ts then ()

  def ignoring[A](t: Any, ts: Any*)(a: A): A =
    if false && t == ts then a
    else a
