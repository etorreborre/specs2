package org.specs2
package specification

import org.specs2.execute
import execute.AsResult
import org.specs2.matcher.{Expectations, DependencyMatchers}

/**
 * This trait provides integrated analysis method for a scala project
 */
trait Analysis extends DependencyMatchers { this: FragmentsBuilder with Expectations =>

  /**
   * this implicit definition allows to check if a Layers definition is respected.
   * It needs to take precedence over the inherited one
   */
  implicit def layersAsResult: AsResult[Layers] = new AsResult[Layers] {
    def asResult(t: =>Layers) = layersToResult(t)
  }

  /**
   * this implicit definition allows to insert a Layers definition directly into the specification, as a Fragment
   */
  implicit def LayersToExample(layers: Layers): Example = layers.toMarkdown ! layers

  /**
   * this implicit definition allows to check if a Layers definition is respected
   */
  implicit def layersToResult(layers: Layers): execute.Result =
    beRespected.apply(createExpectable(layers)).toResult
}