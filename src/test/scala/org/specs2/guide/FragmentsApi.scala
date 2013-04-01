package org.specs2
package guide

class FragmentsApi extends UserGuidePage { def is =
  """
### Presentation

 The interpolated string shown in the [Quick Start](org.specs2.guide.QuickStart.html) is actually desugared as a list of `Fragment` objects joined by the `^` operator:

      "this is my specification" ^
        "and example 1"          ! e1^
        "and example 2"          ! e2

      def e1 = success
      def e2 = success

 What we have here is a list of 3 fragments, a `Text` frgment and 2 `Example` fragments. The examples are declared using the format `"description" ! body`. Their "bodies" are provided by 2 methods returning a `Result`, separated from the specification text.

This section shows ...

### Presentation


  """

}
