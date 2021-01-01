package org.specs2
package text

import collection.BiMap
import BiMap.{given, _}

/**
* This does some simple replacements in sentences to negate them.
*
* For example: the cat must sleep => the cat must not sleep
*/
trait Sentences:

  protected lazy val negationVerbs: BiMap[String, String] =
    BiMap.fromSeq(
      " contains "  <-> " does not contain ",
      " must "      <-> " must not ",
      " must "      <-> " mustn't ",
      " can "       <-> " can not ",
      " can "       <-> " can't ",
      " could "     <-> " could not ",
      " could "     <-> " couldn't ",
      " will "      <-> " will not ",
      " will "      <-> " won't ",
      " was "       <-> " was not ",
      " was "       <-> " wasn't ",
      " were "      <-> " were not ",
      " were "      <-> " weren't ",
      " should "    <-> " should not ",
      " should "    <-> " shouldn't ",
      " shall "     <-> " shall not ",
      " shall "     <-> " shan't ",

      " has "+pr    <-> " does not have "+pr,
      " have "+pr   <-> " do not have "+pr,
      " has "       <-> " has not ",
      " have "      <-> " have not ",
      " has "       <-> " doesn't have ",
      " does "      <-> " does not ",
      " does "      <-> " doesn't ",
      " do "        <-> " do not ",
      " do "        <-> " don't ",
      " is "        <-> " is not ",
      " is "        <-> " isn't ",
      " are "       <-> " are not ",
      " are "       <-> " aren't ",
      " has "       <-> " hasn't ",
      " have "      <-> " haven't "
    )

  protected lazy val negationWords: BiMap[String, String] =
    BiMap.fromSeq(
      "success"  <-> "failure",
      "ok"       <-> "ko"
    )

  /** prefix for a name */
  private lazy val pr = "[a|an|no|the|some|one|two|three|four|five|six|seven|eight|nine|ten|\\d+]"

  /**
   * replace the first occurrence of a verb in the negations table with its negation.
   *
   * We first try to negate an existing negation
   */
  def negateSentence(sentence: String): String =
    def removeRegex(s: String) = s.replaceAll("\\[.+\\]", "")

    // try to match the negations first
    negationVerbs.values.find(v => sentence.matches("(?s).*"+v+".*")).flatMap { negationToReplace =>
      negationVerbs.fromValue(negationToReplace).map(key => sentence.replace(removeRegex(negationToReplace), removeRegex(key)))
    }.orElse {
    // then the positive affirmations
    negationVerbs.keys.find(k =>sentence.matches("(?s).*"+k+".*")).flatMap { toReplace =>
     negationVerbs.fromKey(toReplace).map(value => sentence.replace(removeRegex(toReplace), removeRegex(value)))
    }}.
    // default negated sentence
    getOrElse(defaultNegate(sentence))

  /**
   * replace a word by its negation if found in the dictionary
   */
  def negateWord(word: String): String =
    negationWords.fromKey(word).getOrElse(word)

  /**
   * find the negation of a message by either negating it as a word
   * or a short sentence. If the sentence contains a conjonctive word: or, and, but
   * or if it is a sentence spanning several lines
   * then don't try to negate it
   */
  def negateMessage(message: String): String =
    if List(" and ", " or ", " but ", "\n").exists(w => message.contains(w)) then
      defaultNegate(message)
    else
      negationWords.fromKey(message).getOrElse(negateSentence(message))

  def defaultNegate(sentence: String): String =
    if sentence.startsWith("Expectation unsatisfied: ") then
      sentence.drop("Expectation unsatisfied: ".size)
    else
      s"Expectation unsatisfied: $sentence"

object Sentences extends Sentences
