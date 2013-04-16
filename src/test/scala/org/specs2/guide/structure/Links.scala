package org.specs2
package guide
package structure

object Links extends UserGuideVariables {
  def section = s"""

### Links

There are 2 ways to "link" specifications:

 * by including another specification, to create a parent-child relationship
 * by creating a reference to another specification, to create a peer relationship

#### Inclusion

There is a simple mechanism for including a "children" specification in a given specification. You can simply add the child specification as if it was a simple fragment:

```
s2$triple
This is an included specification
  $$childSpec
$triple
```
Otherwise, if you want to include several specifications at once you can use the `include` method:

```
s2$triple
    These are the included specifications
      $${include(childSpec1, childSpec2, childSpec3)}
$triple
```

The effect of doing so is that all the fragments of the children specification will be inlined in the parent one. This is exactly what is done in some pages of this user guide, but with a twist

```
s2$triple
  $${include(xonly, new GivenWhenThenSpec) }
  $${include(xonly, exampleTextIndentation)}
  $${include(xonly, resetTextIndentation)  }
$triple
```

In the code above there are specific arguments to the included specifications so that they are only displayed when there are failures.

##### Inline

When you include a specification in another one the console will display the beginning and end statistics of the included specification. If you just want to insert the "middle" fragments of the included specification you can use `inline`:

    $${inline(otherSpecification)}

##### Html link

In order to create a User Guide such as this one, you might want the included specification to be written to another html file. In this case, you need a "Link":

    $${link(new QuickStart)}

This declaration will include the child specification so it is executed when the parent specification is executed. However during the reporting, only a Html link will be created in the parent file, referencing a separate file for the children specification. On the other hand if you "hide" the specification, the link will not be printed out:

    $${link((new QuickStart).hide)}

###### Html Link

It is possible to customize the generated Html link with the following syntax:

    $${"a " ~ ("quick start guide", new QuickStart)}

The `~` operator is used to create a `HtmlLink` where:

 * "a" is the beginning of the text
 * "quick start guide" is the text that will be highlighted as a url link
 * `new QuickStart` is the specification to include, the url being derived from the specification class name

Several variations are possible on this pattern, depending which part of the link you want to be highlighted:

    $${"before text" ~ ("text to highlight", specification, "after text")           }
    $${"before text" ~ ("text to highlight", specification, "after text", "tooltip")}
    $${"text to highlight" ~ specification                                          }
    $${"text to highlight" ~ (specification, "after text")                          }
    $${"text to highlight" ~ (specification, "after text", "tooltip")               }

#### Reference

Sometimes you just want to reference another specification without triggering its execution. For example when [creating an index page](#Create+an+index):

    $${see(new MailSenderSpec)}

This will generate a html link in the main specification based on the referenced specification name. If you want to customize that link you can use the following syntax:

    $${"before text" ~/ ("text to highlight", specification, "after text")           }
    $${"before text" ~/ ("text to highlight", specification, "after text", "tooltip")}
    $${"text to highlight" ~/ specification                                          }
    $${"text to highlight" ~/ (specification, "after text")                          }
    $${"text to highlight" ~/ (specification, "after text", "tooltip")               }

#### Markdown url

If you just want to reference the url of the html page that's being generated for a given specification in a paragraph of text, you can use the `markdownUrl` method:

    For more information you can read $${DetailedSpec.markdownUrl}
    // or
    For more information you can read $${DetailedSpec.markdownUrl("the detailed specification")}
    // or
    For more information you can read $${"the detailed specification".markdownUrl(DetailedSpec)}

  """

}
