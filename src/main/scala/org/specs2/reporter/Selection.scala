package org.specs2
package reporter
import specification._

trait Selection {
  val select = (fragments: Fragments) => {
    List(fragments.fragments)
  }
}
