package org.specs2
package reporter
import specification._
import main._
import ExecutedFragments._
import mock.Mockito

class StoringSpec extends SpecificationWithJUnit { def is =
  
   "The statistics of a specification must"                                                        ^
     "be computed after execution"                                                                 ^
       "each executed fragment must have a stats member"                                           ^
         "a Text fragment must have stats = 0"                                                     ! stats.e1^
         "a failed example must have a stats = 1 failure"                                          ! stats.e2^
         "the end of a specification must sum up all the results"                                  ! stats.e3^
                                                                                                   p^bt^
     "be stored"                                                                                   ^
       "stored per specification name"                                                             ! stored.e1^
       "each result also"                                                                          ! stored.e2^
       "and retrieved per specification name"                                                      ! stored.e3^
                                                                                                   endp^
   "It is possible to compute the trends of the statistics"                                        ^
     "between 2 runs"                                                                              ! trends.e1^
     "the trends can be resetted"                                                                  ! trends.e2^
                                                                                                   end

   
  trait Stored extends FragmentExecution with Mockito { outer =>
    val repository = mock[StatisticsRepository]
    val storing = new DefaultStoring {
      override lazy val repository = outer.repository
    }
    implicit val arguments = Arguments() 
    def store(fs: Fragments)(implicit args: Arguments) = storing.store(args <| fs.arguments)(fs.fragments.map(executeFragment)).toList

    repository.getStatistics(any[SpecName]) returns None

  }

   object stats extends Stored {
     def e1 = store("t1").filter(isExecutedText)(0).stats must_== Stats() 
     def e2 = store("e1" ! failure).filter(isExecutedResult)(0).stats must_== Stats(examples = 1, expectations = 1, failures = 1)
     def e3 = store("e0" ! success ^ "e1" ! failure).filter(isExecutedSpecEnd)(0).stats.toString must startWith ( 
       "Stats(examples = 2, successes = 1, expectations = 2, failures = 1")
   }
  
   object stored extends Stored {
     def e1 = {
       store("t1":Fragments)
       there was atLeastOne(repository).storeStatistics(any[SpecName], any[Stats])
     }
     def e2 = {
        store("t1" ^ "e1" ! success)
        there was atLeastOne(repository).storeResults(any[SpecName], any[Seq[ExecutedResult]])
     }
     def e3 = {
        store("t1":Fragments)
        there was atLeastOne(repository).getStatistics(any[SpecName])
     }
   }

   object trends extends Stored {
     def e1 = {
       repository.getStatistics(any[SpecName]) returns Some(Stats(failures = 1))
       store("t1":Fragments).head.stats.trend must_== Some(Stats(failures = -1))
     }
     def e2 = {
       store(args.store(reset=true) ^ "t1")
       there was one(repository).resetStatistics
     }
   }
}