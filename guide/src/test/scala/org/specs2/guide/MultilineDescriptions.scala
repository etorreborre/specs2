package org.specs2
package guide

object MultilineDescriptions extends UserGuidePage { def is = s2"""

In a `s2` string the description of an example is taken as all the text having the same indentation before the example body:${snippet {
s2"""
  This is the introduction paragraph
  Which presents the examples
    the first example has one line $ok

    the second example has
    more than one line             $ok
"""
}}

This prints
```
This is the introduction paragraph
Which presents the examples
  + the first example has one line

  + the second example has
    more than one line
```

If you want the example description to be unevenly aligned you can use a margin `|`:${snippet{
s2"""
  This is the introduction paragraph
  Which presents the examples
    |this example has a very
    | very very
    |   specific indentation $ok
"""
}}

This prints
```
This is the introduction paragraph
Which presents the examples
  + this example has a very
     very very
       specific indentation
```


"""
}
