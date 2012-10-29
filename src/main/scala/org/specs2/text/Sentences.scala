package org.specs2
package text

import collection.BiMap
import BiMap._

/**
* This does some simple replacements in sentences to negate them.
*
* For example: the cat must sleep => the cat must not sleep
*/
trait Sentences {

  protected lazy val negationsTable: BiMap[String, String] =
    Seq(
      "must"      <-> "must not",
      "must"      <-> "mustn't",
      "can"       <-> "can not",
      "can"       <-> "can't",
      "could"     <-> "could not",
      "could"     <-> "couldn't",
      "will"      <-> "will not",
      "will"      <-> "won't",
      "was"       <-> "was not",
      "was"       <-> "wasn't",
      "were"      <-> "were not",
      "were"      <-> "weren't",
      "should"    <-> "should not",
      "should"    <-> "shouldn't",
      "shall"     <-> "shall not",
      "shall"     <-> "shan't",

      "has "+pr   <-> "does not have "+pr,
      "have "+pr  <-> "do not have "+pr,
      "has"       <-> "has not",
      "have"      <-> "do not have",
      "has"       <-> "doesn't have",
      "does"      <-> "does not",
      "does"      <-> "doesn't",
      "do"        <-> "do not",
      "do"        <-> "don't",
      "is"        <-> "is not",
      "is"        <-> "isn't",
      "are"       <-> "are not",
      "are"       <-> "aren't",
      "has"       <-> "hasn't",
      "have"      <-> "haven't"
    )

  /** prefix for a name */
  private lazy val pr = "[a|an|no|the|some|one|two|three|four|five|six|seven|eight|nine|ten|\\d+]"

  /**
   * replace the first occurrence of a verb in the negations table with its negation.
   *
   * We first try to negate an existing negation
   */
  def negateSentence(sentence: String): String = {
    def removeRegex(s: String) = s.replaceAll("\\[.+\\]", "")

    // try to match the negations first
    negationsTable.values.find(v => sentence.matches(".*"+v+".*")).flatMap { negationToReplace =>
      negationsTable.fromValue(negationToReplace).map(key => sentence.replace(removeRegex(negationToReplace), removeRegex(key)))
    }.orElse {
    // then the positive affirmations
    negationsTable.keys.find(k =>sentence.matches(".*"+k+".*")).flatMap { toReplace =>
     negationsTable.fromKey(toReplace).map(value => sentence.replace(removeRegex(toReplace), removeRegex(value)))
    }}.
    // default to not(sentence)
    getOrElse("not("+sentence+")")
  }
}

