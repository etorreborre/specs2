package org.specs2
package text

import mutable.{Tables, Specification}

class SentencesSpec extends Specification with Tables with Sentences {

  "It is possible to create the negation of a sentence" >> {
    "sentence"             | "negated"               | "reversible" |>
    "it has a"             ! "it does not have a"    ! true         |
    "it is a"              ! "it is not a"           ! true         |
    "it is a\n"            ! "it is not a\n"         ! true         |
    "they are a"           ! "they are not a"        ! true         |
    "it does a"            ! "it does not a"         ! true         |
    "it has changed"       ! "it has not changed"    ! true         |
    "it will do"           ! "it will not do"        ! true         |
    "it was a"             ! "it was not a"          ! true         |
    "they were a"          ! "they were not a"       ! true         |
    "it can do"            ! "it can not do"         ! true         |
    "it could do"          ! "it could not do"       ! true         |
    "it must do"           ! "it must not do"        ! true         |
    "it should do"         ! "it should not do"      ! true         |
    "they have a"          ! "they do not have a"    ! true         |
    "they have been"       ! "they have not been"    ! true         |
    "it isn't a"           ! "it is a"               ! false        |
    "they aren't a"        ! "they are a"            ! false        |
    "it does a"            ! "it does not a"         ! true         |
    "it does doodoo"       ! "it does not doodoo"    ! true         |
    "it does not have a"   ! "it has a"              ! false        |
    "it won't do"          ! "it will do"            ! false        |
    "it wasn't a"          ! "it was a"              ! false        |
    "they weren't a"       ! "they were a"           ! false        |
    "it can't do"          ! "it can do"             ! false        |
    "it couldn't do"       ! "it could do"           ! false        |
    "it mustn't do"        ! "it must do"            ! false        |
    "it shouldn't do"      ! "it should do"          ! false        |
    "it hasn't a"          ! "it has a"              ! false        |
    "they haven't a"       ! "they have a"           ! false        |  { (sentence, negated, isReversible) =>

      "the sentence is negated"            ==> (negateSentence(sentence) === negated)
      "the sentence can be double-negated" ==> (negateSentence(negateSentence(sentence)) === sentence).when(isReversible)
    }
  }
}
