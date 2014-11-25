package org.specs2
package guide

object HowTo extends UserGuidePage { def is = s2"""

 Question                                                                                | See
---------------------------------------------------------------------------------------- | ------
How to execute an action before all the examples?                                        | ${link(Contexts)}
How to execute an action before each example?                                            | ${link(Contexts)}
Can I create and reuse a Specification "template"?                                       | ${link(SpecificationTemplate)}
How can I pass some data to each example?                                                | ${link(Contexts)}
Can I add more information to my results for easier diagnostic?                          | ${link(ExpectationDescription)}
Is it possible to execute a Specification sequentially?                                  | ${link(Execution)}
How to mark an example as `pending` until it is fixed?                                   | ${link(PendingUntilFixedExamples)}
Can I simply mark a block of code as `pending`?                                          | ${link(StandardResults)}
Is there a way to run only one example?                                                  | ${link(Selection)}
Can I skip examples?                                                                     | ${link(SkipExamples)}
How can I tag examples?                                                                  | ${link(Selection)}
Is it possible to modify the behaviour of a Specification with command-line arguments?   | ${link(UseCommandLineArguments)}
How to capture snippets of code and add them to my Specification?                        | ${link(CaptureSnippets)}
Can I create an HTML index of all the specifications?                                    | ${link(HtmlOutput)}
Is it possible to execute a Specification in a random order?                             | ${link(RandomExecution)}
How can I easily print expressions to the console?                                       | ${link(DebugStatements)}
Can $specs2 create a JUnit xml file for a continuous integration server?                 | ${link(JUnitXmlOutput)}
Is it possible to reduce the number of implicits in scope and improve compilation times? | ${link(LightweightSpecs)}
How can I collect execution data?                                                        | ${link(PrintExecutionData)}
Can I create an example with just some code?                                             | ${link(AutoExamples)}
Help, I need to troubleshoot my issues!                                                  | ${link(Troubleshooting)}

"""
}
