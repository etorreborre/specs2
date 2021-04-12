package org.specs2
package guide

object JUnitXmlOutput extends UserGuidePage { def is = "JUnit XML output".title ^ s2"""
Many continuous integration servers (like [Jenkins](http://jenkins-ci.org)) accept JUnit XML as their de facto standard for reporting test results. You can output a JUnit XML file by simply using the `junitxml` argument:

 `testOnly org.acme.MySpec -- junitxml`

The JUnit XML file corresponding to the specification will be produced by default in the `target/test-reports` directory. You can change this directory by passing the `junit.outdir` argument like so:

 `testOnly org.acme.MySpec -- junitxml junit.outdir custom_xml_folder`

This will output the xml files in the `custom_xml_folder` in the top level project directory.

${"Remember" ~/ ConsoleOutput} that using `junitxml` by itself will turn off the console reporting. You need to add `console` to get it back.

Note that `sbt test` does not take parameters so the default behavior is to produce the JUnit XML files for all specifications in the default output directory

In order to change the default output directory of the junit xml files when running the tests with `test`, add the following to your sbt build file:

 `testOptions in Test += Tests.Argument("junitxml", "junit.outdir", "custom_xml_folder")`

Note that this will suppress the console output, which may be what you want if this is a configuration for a build machine. To re-enable console output, use instead:

 ```
 testOptions in Test ++= Seq(
    Tests.Argument("junitxml", "junit.outdir", "custom_xml_folder"),
    Tests.Argument("console")
 )
 ```

"""
}

