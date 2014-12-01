package org.specs2

import org.specs2.specification.create.DefaultFragmentFactory

/** this package object is just here for compatibility reasons */
package object specification {
  @deprecated(message = "use step instead", since = "3.0")
  def Step(a: =>Any) =
    DefaultFragmentFactory.step(a)

  @deprecated(message = "use step instead", since = "3.0")
  def Step(a: =>Any, global: Boolean = false) =
    DefaultFragmentFactory.step(a).makeGlobal(global)

  @deprecated(message = "use action instead", since = "3.0")
  def Action(a: =>Any) =
    DefaultFragmentFactory.action(a)
}
