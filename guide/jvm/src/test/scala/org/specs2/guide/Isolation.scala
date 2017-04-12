package org.specs2
package guide

import java.io.{PrintWriter, ByteArrayOutputStream}
import java.util.Properties

import scala.collection.JavaConversions._

object Isolation extends UserGuidePage { def is = s2"""

Unit specifications allow to nest blocks, as if they were different contexts, going from the more general to the more specific. It can be very tempting, for some applications, to include some mutable state representing data in more and more specific contexts. However, this can be problematic because:

 - you have to reset the state for each example in a sequential specification
 - you will have concurrency issues for a concurrent one

One way to avoid this is to execute each example in its own instance of the specification.

### Isolated variables

The `isolated` argument duplicates the body of each example so that it is executed in a brand new instance of the Specification: ${snippet{

class IsolatedSpec extends mutable.Specification {
  isolated

  "Java Properties can store configuration properties" >> {
    // empty properties context
    val props = new Properties
    "we can add a property" >> {
      props.setProperty("p1", "v1")
      props.getProperty("p1") must_== "v1"
    }
    "we can get all the properties names" >> {
      props.setProperty("p1", "v1")
      props.setProperty("p2", "v2")
      props.propertyNames.toList must_== List("p1", "p2")
    }
    "Properties can be stored" >> {
      // non-empty properties context
      props.setProperty("p1", "v1")
      props.setProperty("p2", "v2")

      "to an OutputStream" >> {
        val out = new ByteArrayOutputStream
        props.store(out, "to stream")
        out.size() must be_>(0)
      }
      "to a Writer" >> {
        val out = new ByteArrayOutputStream
        val writer = new PrintWriter(out)
        props.store(writer, "to writer")
        out.size() must be_>(0)

      }
    }
  }
}
}}

Since there is a new Specification for each example, then all the variables accessible to the example will be seen as new.

### Instantiation

If the specification has a constructor with parameters, they need themselves to be instantiable with a no-args constructor. For example `class AkkaSpec(system: AkkaSystem) extends Specification` can not be instantiated because `AkkaSystem` doesn't have a 0-args constructor.

"""
}
