package org.specs2
package specification

import _root_.org.specs2.mutable.{Specification => Spec}

class SpecNameSpec extends Spec {
  
  "A spec name can be built from a single string" >> { 
    val specName = SpecificationTitle("t")
    
    "it has a title" >> { specName.title must_== "t" }
    "it has a name" >> { specName.name must_== "t" }
  }
  
  "A spec name can be built from a specification instance" >> {
    val specName = SpecificationName(new TestSpecification)
    
    "it has a title" >> { specName.title must_== "TestSpecification" }
    "it has a name" >> { specName.name must_== "TestSpecification" }
  }
  
  "A specification title can be overriden by a specification name" >> {
    val specName = SpecificationTitle("t").overrideWith(SpecificationName(new TestSpecification))
    
    "the title is the title of the specification title" >> {
      specName.title must_== "t"
    }
    
    "the name is the name of the specification name" >> {
      specName.name must_== "TestSpecification"
    }
  }

  "A specification name can be overriden by a specification title" >> {
    val specName = SpecificationName(new TestSpecification).overrideWith(SpecificationTitle("t"))
    
    "the title is the title of the specification title" >> {
      specName.title must_== "t"
    }
    
    "the name is the name of the specification name" >> {
      specName.name must_== "TestSpecification"
    }
  }
  
  "A Fragments object can have both a name and title" >> {
    val spec = new Specification { def is = "title".title ^ "text" }
    spec.content.specStart.title must_== "title"
    spec.content.specStart.name must_== "Specification"
  }

  "A SpecName can define a url" >> {
    SpecName(this).url must endWith(getClass.getName + ".html")
  }

  "A SpecName can define a markdown link for its url" >> {
    "for a normal one" >> {
      SpecName(this).markdownLink.toString must_== "[SpecNameSpec](org.specs2.specification.SpecNameSpec.html)"
    }
    "when the title contains spaces" >> {
      SpecificationTitle("hello world").markdownLink.toString must_== "[hello world](hello%20world.html)"
    }
    "with a specific title" >> {
      SpecName(this).markdownLink("the spec").toString must_== "[the spec](org.specs2.specification.SpecNameSpec.html)"
    }
  }
}

class TestSpecification extends Specification { def is  = "e1" ! success }
