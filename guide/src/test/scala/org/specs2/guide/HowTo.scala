package org.specs2
package guide

object HowTo extends UserGuidePage { def is = s2"""

 Question                                                                                | See
---------------------------------------------------------------------------------------- | ------
How to execute an action before all the examples?                                        | ${link(Contexts).mute}
How to execute an action before each example?                                            | ${link(Contexts).mute}
Can I create and reuse a Specification "template"?                                       | ${link(SpecificationTemplate).mute}
How can I pass some data to each example?                                                | ${link(Contexts).mute}
Can I add more information to my results for easier diagnostic?                          | ${link(ExpectationDescription).mute}
Is it possible to execute a Specification sequentially?                                  | ${link(Execution).mute}
How to mark an example as `pending` until it is fixed?                                   | ${link(PendingUntilFixedExamples).mute}
Can I simply mark a block of code as `pending`?                                          | ${link(StandardResults).mute}
Is there a way to run only one example?                                                  | ${link(Selection).mute}
I would like to display the execution time of each example                               | ${link(ConsoleOutput).mute}
Can I use a for loop to create examples or results?                                      | ${link(ForLoops).mute}
How can I tag examples?                                                                  | ${link(Selection).mute}
Can I skip examples?                                                                     | ${link(SkipExamples).mute}
Is it possible to modify the behaviour of a Specification with command-line arguments?   | ${link(UseCommandLineArguments).mute}
How to capture snippets of code and add them to my Specification?                        | ${link(CaptureSnippets).mute}
Can I create an HTML index of all the specifications?                                    | ${link(HtmlOutput).mute}
Is it possible to execute a Specification in a random order?                             | ${link(RandomExecution).mute}
How can I easily print expressions to the console?                                       | ${link(DebugStatements).mute}
Can $specs2 create a JUnit xml file for a continuous integration server?                 | ${link(JUnitXmlOutput).mute}
Is it possible to reduce the number of implicits in scope and improve compilation times? | ${link(LightweightSpecs).mute}
How can I collect execution data?                                                        | ${link(PrintExecutionData).mute}
Can I create an example description spanning several lines?                              | ${link(MultilineDescriptions).mute}
How to isolate each example from another so that they don't share variables              | ${link(Isolation).mute}
Help, I need to troubleshoot my issues!                                                  | ${link(Troubleshooting).mute}

"""
}
