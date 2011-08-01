package org.specs2
package reporter
import specification._
import main._
import ExecutedFragments._
import mock.Mockito

class StoringSpec extends SpecificationWithJUnit { def is =
  
   "The statistics of a specification must"                                             ^
     "be computed after execution"                                                      ^
       "each executed fragment must have a stats member"                                ^
         "a Text fragment must have stats = 0"                                          ! stats.e1^
         "a failed example must have a stats = 1 failure"                               ! stats.e2^
         "the end of a specification must sum up all the results"                       ! stats.e3^
     "be stored"                                                                        ^
       "stored per specification name"                                                  ! stored.e1^
       "and retrieved per specification name"                                           ! stored.e2^
                                                                                        endp^
   "It is possible to compute the trends of the statistics"                             ^
     "between 2 runs"                                                                   ! trends.e1^
     "the trends can be resetted"                                                       ! trends.e2^
                                                                                        endp^
   "It is possible to re-run"                                                           ^
     "failed specifications only with the 'failedspec' argument"                        ! rerun.e1^
     "failed examples only with the 'failedexample' argument"                           ! rerun.e2^
                                                                                        end

   
  trait Stored extends FragmentExecution with Mockito { outer =>
    val repository = mock[StatisticsRepository]
    val storing = new DefaultStoring {
      override lazy val repository = outer.repository
    }
    implicit val arguments = Arguments() 
    def store(fs: Fragments) = storing.store(arguments)(fs.fragments.map(executeFragment))
  }                                                                                        
   
   
   object stats extends Stored {
     def e1 = store("t1").filter(isExecutedText)(0).stats must_== Stats() 
     def e2 = store("e1" ! failure).filter(isExecutedResult)(0).stats must_== Stats(fragments = 1, expectations = 1, failures = 1) 
     def e3 = store("e0" ! success ^ "e1" ! failure).filter(isExecutedSpecEnd)(0).stats.toString must startWith ( 
       "Stats(fragments = 2, successes = 1, expectations = 2, failures = 1")
   }
  
   object stored extends Stored {
     def e1 = {
       store("t1":Fragments)
       there was one(repository).storeStatistics(any[SpecName], any[Stats])
     }
     def e2 = {
        store("t1":Fragments)
        there was one(repository).getStatistics(any[SpecName])
     }
   }

   object trends extends Stored {
     def e1 = {
       repository.getStatistics(any[SpecName]) returns Stats(failures = 1)
       store("t1":Fragments).head.stats.trend must_== Stats(failures = -1)
     }
     def e2 = pending
   }

   object rerun extends Stored {
     def e1 = pending
     def e2 = pending
   }
}