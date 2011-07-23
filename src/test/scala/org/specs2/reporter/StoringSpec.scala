package org.specs2
package reporter
import specification._
import main._
import ExecutedFragments._

class StoringSpec extends SpecificationWithJUnit { def is =
  
   "The statistics of a specification must"                                             ^
     "be computed after execution"                                                      ^
       "each executed fragment must have a stats member"                                ^
         "a Text fragment must have stats = 0"                                          ! stats.e1^
         "a failed example must have a stats = 1 failure"                               ! stats.e2^
         "the end of a specification must sum up all the results"                       ! stats.e3^
     "be stored"                                                                        ^
       "per specification name"                                                         ! e2^
                                                                                        end

   
  trait Stored extends FragmentExecution {
    val storing = new DefaultStoring {}
    implicit val arguments = Arguments() 
    def store(fs: Fragments) = storing.store(arguments)(fs.fragments.map(executeFragment))
  }                                                                                        
   
   
   object stats extends Stored {
     def e1 = store("t1").filter(isExecutedText)(0).stats must_== Stats() 
     def e2 = store("e1" ! failure).filter(isExecutedResult)(0).stats must_== Stats(fragments = 1, expectations = 1, failures = 1) 
     def e3 = store("e0" ! success ^ "e1" ! failure).filter(isExecutedSpecEnd)(0).stats must_== 
       Stats(fragments = 2, expectations = 2, successes = 1, failures = 1) 
  
   }
     
   def e2 = pending
}