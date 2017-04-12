package org.specs2
package specification

import process._
import StoreKeys._
import time.SimpleTimer
import matcher._

class StoreKeysSpec extends Spec with TypedEqual { def is = s2"""

 The StoreKeys object can
   resolve file names based on the key $e1
   encode/decode stats $e2

"""

  def e1 = {
    val specName = getClass.getName
    val key = SpecificationStatsKey(specName)

    resolve(key) === s"$specName.stats"
  }

  def e2 = {
    val trend = Stats(0, 1, 2, 3, 4, 4, 6, 7, None, new SimpleTimer)
    val s1 = Stats(0, 1, 2, 3, 4, 4, 6, 7, Some(trend), new SimpleTimer)
    val key = SpecificationStatsKey("")
    decode(key, encode(key, s1)) === Some(s1)
  }
}
