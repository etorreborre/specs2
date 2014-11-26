package org.specs2.guide

object DetailedTopics extends UserGuidePage { def is = s2"""

 Topic                                                                                   | See
---------------------------------------------------------------------------------------- | ------
Create an example with just some code as the description                                 | ${link(AutoExamples)}
Use the Given/When/Then style                                                            | ${link(GivenWhenThenStyle)}
Gather all expectations in an example                                                    | ${link(GetAllExpectations)}
Reference another specification                                                          | ${link(ReferenceOtherSpecifications)}
Add formatting to a specification                                                        | ${link(SpecificationFormatting)}
Create example names automatically in an acceptance specification                        | ${link(NamedExamples)}
Infer example descriptions from text in an acceptance specification                      | ${link(AutoNumberedExamples)}
Create HTML tables to specify examples                                                   | ${link(UseForms)}
Create examples "on the fly" as the specification executes                               | ${link(CreateOnlineSpecifications)}
Create an example with different data displayed in a table                               | ${link(UseDatatables)}
Fragments API                                                                            | ${link(FragmentsApi)}
Create a new type of Result                                                              | ${link(AsResultTypeclass)}
Other build tools                                                                        | ${link(OtherBuildTools)}
Syntax variations with matchers                                                          | ${link(matchers.SyntacticVariations)}
Matchers for case classes                                                                | ${link(matchers.CaseClassMatchers)}
Matchers for packages dependencies                                                       | ${link(matchers.DependencyMatchers)}
Use $specs2 matchers outside of $specs2                                                  | ${link(matchers.OutsideSpecs2)}
Matchers reference card                                                                  | ${link(matchers.ReferenceCard)}
Arguments reference card                                                                 | ${link(ArgumentsReference)}
"""

}
