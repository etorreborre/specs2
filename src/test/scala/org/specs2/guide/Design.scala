package org.specs2
package guide

class Design extends Specification { def is = freetext                                                                  ^
  "specs2 design".title ^
                                                                                                                        """
<toc/>
                                                                                                                        
### Presentation

This page explains the design of _specs2_:

 * the structure of a specification
 * how the specification is built
 * how the specification is executed
 * how the reporting works

### Specification structure

The structure of a specification is very simple, it is just a list of `Fragments` provided by the `is` method of the
`SpecificationStructure` trait:

      +---------------+         1..n   +-----------+
      | Specification | -------------> | Fragment  |
      +---------------+                +-----------+
                                             ^
                                             |
             +----------+-----------+-----------+-------------+-------------+----------+                                          
             |          |           |           |             |             |          |
         +------+  +---------+  +-------+  +---------+  +-----------+  +---------+  +------+
         | Text |  | Example |  | Step  |  | Action  |  | SpecStart |  | SpecEnd |  | See  |
         +------+  +---------+  +-------+  +---------+  +-----------+  +---------+  +------+

Here's a short description of all Fragments:

 * Text: free text describing the specified system
 * Example: a description and a piece of executable code returning a Result


                                                                                                                        """^
                                                                                                                        end
}