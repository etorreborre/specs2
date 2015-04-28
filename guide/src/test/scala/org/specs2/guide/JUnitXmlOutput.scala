package org.specs2
package guide

object JUnitXmlOutput extends UserGuidePage { def is = "JUnit XML output".title ^ s2"""
Many continuous integration servers (like [Jenkins](http://jenkins-ci.org)) accept JUnit XML as their de facto standard for reporting test results. You can output a JUnit XML file by simply using the `junitxml` argument:

 `testOnly org.acme.MySpec -- junitxml`

The JUnit XML file corresponding to the specification will be produced by default in the `target/test-reports` directory. You can change this directory by passing the `junit.outdir` argument.

${"Remember" ~/ ConsoleOutput} that using `junitxml` by itself will turn off the console reporting. You need to add `console` to get it back.

"""
}

