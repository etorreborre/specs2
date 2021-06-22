package org.specs2
package fp

import org.specs2.control.producer.Producer

class ProducerSpec extends Specification { def is = s2"""

 emitting a large sequence of elements with emitSeq must not stack overflow $stackOverflow

"""

  def stackOverflow = {
    Producer.emitSeq(List.range(1, 10000))
    ok
  }

}
