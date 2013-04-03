package org.specs2
package specification

class SnippetsSpec extends Specification with Snippets { def is = s2"""
  this is some code : ${ snippet {

  s2"""
     test $ok
     test1 $ok
  """
}

}
                                                    """
}
