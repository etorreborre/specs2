package org.specs2
package specification

import execute.AsResult
import matcher.{Expectations, DependencyMatchers}
import org.specs2.specification.core.{Fragments, Fragment}
import org.specs2.specification.dsl.FragmentDsl
import specification.create.FragmentsFactory

/**
 * This trait provides integrated analysis method for a scala project
 */
trait Analysis extends DependencyMatchers with FragmentDsl { this: FragmentsFactory with Expectations =>

  /**
   * this implicit definition allows to check if a Layers definition is respected.
   * It needs to take precedence over the inherited one
   */
  implicit def layersAsResult: AsResult[Layers] = new AsResult[Layers] {
    def asResult(t: =>Layers) = layersToResult(t)
  }

  implicit class appendLayersToString(s: String) extends appendToString(s) {
    def ^(layers: Layers): Fragments = ^(LayersToExample(layers))
  }

  /**
   * this implicit definition allows to insert a Layers definition directly into the specification, as a Fragment
   */
  implicit def LayersToExample(layers: Layers): Fragment =
    fragmentFactory.Example(layers.toMarkdown, layers)

  /**
   * this implicit definition allows to check if a Layers definition is respected
   */
  implicit def layersToResult(layers: Layers): execute.Result =
    beRespected.apply(createExpectable(layers)).toResult
}