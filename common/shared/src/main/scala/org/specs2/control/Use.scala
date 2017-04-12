package org.specs2.control

/**
 * Utility object to show a parameter as used
 * In particular this is useful to avoid warnings with ImplicitParameters
 */
private[specs2]
object Use {

  def apply(t: Any, ts: Any*): Unit =
    if (false && t == ts) ()

  def ignoring[A](t: Any, ts: Any*)(a: A): A =
    if (false && t == ts) a
    else a

}
