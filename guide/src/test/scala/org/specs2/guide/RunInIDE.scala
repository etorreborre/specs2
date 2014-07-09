package org.specs2
package guide

object RunInIDE extends UserGuidePage { def is = s2"""

## Intellij IDEA

[IntelliJ IDEA](http://www.jetbrains.com/idea/features/scala.html) is the IDE with the best $specs2 integration for now. You can:

 * execute a specification by selecting its name and pressing `CTRL+SHIFT+F10`
 * execute a single example by selecting its description and pressing `CTRL+SHIFT+F10`

 ![specs2 in Intellij](${IMAGES_DIR}intellij.png)

However passing arguments needs to be done through system properties for now. So if you need to use the `xonly` argument you need to pass `-Dspecs2.xonly`.

## Scala IDE

There is no integration of $specs2 in [ScalaIDE](http://scala-ide.org) yet, but it is possible to execute specifications as JUnit tests:${snippet{
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MySpecification extends org.specs2.Specification { def is = s2"""
  Define your specification as usual here ...
"""
}
}}

[*some [tricks](http://code.google.com/p/specs/wiki/RunningSpecs#Run_your_specification_with_JUnit4_in_Eclipse) described on the specs website can still be useful there when ScalaIDE struggles to find the specification classes*]
"""
}


