package org.specs2
package guide

object HowTo extends UserGuidePage { def is = s2"""

 Question                                                                                | See
---------------------------------------------------------------------------------------- | ------
How to execute an action before all the examples?                                        | ${see(Contexts)}
How to execute an action before each example?                                            | ${see(Contexts)}
Can I create and reuse a Specification "template"?                                       | ${see(SpecificationTemplate)}
How can I pass some data to each example?                                                | ${see(Contexts)}
Can I add more information to my results for easier diagnostic?                          | ${see(ExpectationDescription)}
Is it possible to execute a Specification sequentially?                                  | ${see(Execution)}
How to mark an example as `pending` until it is fixed?                                   | ${see(PendingUntilFixedExamples)}
Can I simply mark a block of code as `pending`?                                          | ${see(StandardResults)}
Is there a way to run only one example?                                                  | ${see(Selection)}
Can I skip examples?                                                                     | ${see(SkipExamples)}
How can I tag examples?                                                                  | ${see(Selection)}
Is it possible to modify the behaviour of a Specification with command-line arguments?   | ${see(UseCommandLineArguments)}
How to capture snippets of code and add them to my Specification?                        | ${see(CaptureSnippets)}
Can I create an HTML index of all the specifications?                                    | ${see(HtmlOutput)}
Is it possible to execute a Specification in a random order?                             | ${see(RandomExecution)}
How can I easily print expressions to the console?                                       | ${see(DebugStatements)}
Can $specs2 create a JUnit xml file for a continuous integration server?                 | ${see(JUnitXmlOutput)}
Is it possible to reduce the number of implicits in scope and improve compilation times? | ${see(LightweightSpecs)}
How can I collect execution data?                                                        | ${see(PrintExecutionData)}
Help, I need to troubleshoot my issues!                                                  | ${see(Troubleshooting)}

"""
}
