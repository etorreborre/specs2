package org.specs2
package text

import mutable.{Tables, Specification}

class SentencesSpec extends Specification with Tables with Sentences {

  "It is possible to create the negation of a sentence" >> {
    "sentence"             | "negated"               | "reversible" |>
    "it has a"             ! "it does not have a"    ! true         |
    "it is a"              ! "it is not a"           ! true         |
    "they are a"           ! "they are not a"        ! true         |
    "it does a"            ! "it does not a"         ! true         |
    "it has changed"       ! "it has not changed"    ! true         |
    "it will do"           ! "it will not do"        ! true         |
    "it can do"            ! "it can not do"         ! true         |
    "it could do"          ! "it could not do"       ! true         |
    "it must do"           ! "it must not do"        ! true         |
    "it should do"         ! "it should not do"      ! true         |
    "they have a"          ! "they do not have a"    ! true         |
    "it isn't a"           ! "it is a"               ! false        |
    "they aren't a"        ! "they are a"            ! false        |
    "it does a"            ! "it does not a"         ! true         |
    "it does not have a"   ! "it has a"        ! false        |
    "it won't do"          ! "it will do"            ! false        |
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
