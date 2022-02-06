package org.specs2
package mutable

import control._
import org.specs2.specification.core.Fragments
import org.specs2.specification.core.mutable._
import org.specs2.specification.create._

/**
 * This trait adds the possibility to execute the before behavior before the body of the context.
 */
trait Before extends SpecificationStructure with org.specs2.specification.Before with FragmentsFactory {
  override def map(fs: =>Fragments) =
    super.map(fs.flatMap(f => emitAsync(fragmentFactory.step(before), f)))
}
