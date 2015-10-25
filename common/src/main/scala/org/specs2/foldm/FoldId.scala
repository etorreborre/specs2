package org.specs2
package foldm

import java.security.MessageDigest

import FoldM._
import scalaz.{\/, \/-, -\/, Order}
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.syntax.foldable._
import scalaz.Id._

/**
 * List of predefined FoldIds
 */
object FoldId {

  /** @return fold to count elements */
  def count[T]: FoldState[T, Int] =
    fromMonoidMap(_ => 1)

  /** @return fold to count the number of unique elements */
  def countUnique[T]: Fold[T, Int] = new FoldM[T, Id, Int] {
    type S = scala.collection.mutable.HashSet[T]
    def start = new scala.collection.mutable.HashSet[T]
    def fold = (s: S, t: T) => { s.add(t); s }
    def end(s: S) = s.size
  }

  /** @return return false if the list is empty or if all elements are false, use a \/- state to indicate early success */
  def any[T](f: T => Boolean) = new FoldM[T, Id, Boolean] {
    type S = Boolean \/ Boolean
    def start = -\/(false)
    def fold = (s: S, t: T) => if (f(t)) \/-(true) else s
    def end(s: S) = s.fold(b => b, b => b)
  }

  /** @return return true if the list is empty or if all elements are true, use a \/- state to indicate early failure */
  def all[T](f: T => Boolean) = new FoldM[T, Id, Boolean] {
    type S = Boolean \/ Boolean
    def start = -\/(true)
    def fold = (s: S, t: T) => if (!f(t)) \/-(false) else s
    def end(s: S) = s.fold(b => b, b => b)
  }

  /** @return the last element */
  def last[T]: FoldState[T, Option[T]] =
    fromFoldLeft[T, Option[T]](None)((u, t) => Some(t))

  /** @return the latest n elements */
  def latest[T](n: Int) = new FoldM[T, Id, List[T]] {
    type S = scala.collection.mutable.ListBuffer[T]
    def start = new scala.collection.mutable.ListBuffer[T]
    def fold = (s: S, t: T) => { s.append(t); if (s.size > n) s.remove(0); s }
    def end(s: S) = s.toList
  }

  /** lift a function to a fold that applies f to the last element */
  def lift[T, U](f: T => U) =
    last[T] map ((_:Option[T]).map(f))

  /** @return a plus fold from a Num */
  def plus[N : Numeric]: FoldState[N, N] =
    fromFoldLeft(implicitly[Numeric[N]].zero)(implicitly[Numeric[N]].plus)

  /** @return a plus fold from a mapping to a Num */
  def plusBy[A, N : Numeric](f: A => N): FoldState[A, N] =
    plus[N].contramap[A](f)

  /** @return a times fold from a Num */
  def times[N : Numeric]: FoldState[N, N] =
    fromFoldLeft(implicitly[Numeric[N]].zero)(implicitly[Numeric[N]].times)

  /** @return a times fold from a mapping to a Num */
  def timesBy[A, N : Numeric](f: A => N): FoldState[A, N]  =
    times[N].contramap[A](f)

  /** @return a maximum fold */
  def maximum[T : Order]: FoldState[T, Option[T]] =
    fromFoldLeft(None:Option[T])((m: Option[T], t: T) => (m.toList :+ t).maximum)

  /** @return a maximum fold by a given member */
  def maximumBy[A, T : Order](f: A => T): FoldState[A, Option[A]] =
    fromFoldLeft(None:Option[A])((m: Option[A], a: A) => (m.toList :+ a).maximumBy(f))

  /** @return a maximum fold of a given member */
  def maximumOf[A, T : Order](f: A => T): FoldState[A, Option[T]] =
    maximum[T].contramap[A](f)

  /** @return a minimum fold */
  def minimum[T : Order]: FoldState[T, Option[T]] =
    fromFoldLeft(None:Option[T])((m: Option[T], t: T) => (m.toList :+ t).minimum)

  /** @return a minimum fold by a given member */
  def minimumBy[A, T : Order](f: A => T): FoldState[A, Option[A]] =
    fromFoldLeft(None:Option[A])((m: Option[A], a: A) => (m.toList :+ a).minimumBy(f))

  /** @return a minimum fold of a given member */
  def minimumOf[A, T : Order](f: A => T): FoldState[A, Option[T]] =
    minimum[T].contramap[A](f)

  /** @return the mean of elements */
  def mean[N : Fractional]: Fold[N, N] { type S = (N, Int) } =
    plus.zip(count).map { case (s, c) =>
      val frac = implicitly[Fractional[N]]; import frac._

      if (c == 0) frac.zero
      else        s / fromInt(c)
    }

  /** @return the number of elements, mean and standard deviation */
  def stddev[N : Fractional]: Fold[N, Double] =
    onlineStddev.map(_._3)

  /** @return the number of elements, mean and standard deviation */
  def onlineStddev[N : Fractional]: Fold[N, (Int, N, Double)] =
    onlineVariance map { case (count, mean, variation) =>
      implicit val num = implicitly[Fractional[N]]; import num._
      (count, mean, math.sqrt(toDouble(variation)))
    }

  /** @return the number of elements, mean and variance */
  def onlineVariance[N : Fractional]: Fold[N, (Int, N, N)] =
    onlineVariation map { case (count, mean, variation) =>
      implicit val num = implicitly[Fractional[N]]; import num._

      if (count <= 1) (count, mean, variation)
      else            (count, mean, variation / fromInt(count))
    }

  /** @return the number of elements, mean and unbiased variance */
  def onlineUnbiasedVariance[N : Fractional]: Fold[N, (Int, N, N)] =
    onlineVariation map { case (count, mean, variation) =>
      implicit val num = implicitly[Fractional[N]]; import num._

      if (count <= 1) (count, mean, variation)
      else            (count, mean, variation / fromInt(count - 1))
    }

  /** @return the number of elements, mean and variation */
  def onlineVariation[N : Fractional]: Fold[N, (Int, N, N)] = new Fold[N, (Int, N, N)] {
    implicit val num = implicitly[Fractional[N]]; import num._
    type S = (Int, N, N)
    def start = (0, num.zero, num.zero)

    def fold = (s: S, n: N) => {
      val (count, mean, variation) = s

      val count1 = count + 1
      val delta = n - mean
      val mean1 = mean + delta / fromInt(count1)
      val variation1 = variation + (delta * (n - mean1))

      (count1, mean1, variation1)
    }

    def end(s: S) = s
  }

  /** @return a Fold which simply accumulates elements into a List */
  def list[T]: Fold[T, List[T]] = new Fold[T, List[T]] {
    // a ListBuffer is used for efficient appends
    type S = scala.collection.mutable.ListBuffer[T]
    def start = new scala.collection.mutable.ListBuffer[T]
    def fold = (s: S, t: T) => { s.append(t); s }
    def end(s: S) = s.toList
  }

  /** checksums */
  // read bytes, an array of bytes + the number of bytes read
  type Bytes = (Array[Byte], Int)

  def md5: Fold[Array[Byte], String] =
    checksum("MD5")

  def sha1: Fold[Array[Byte], String] =
    checksum("SHA1")

  def bytesMd5: Fold[Bytes, String] =
    bytesChecksum("MD5")

  def bytesSha1: Fold[Bytes, String] =
    bytesChecksum("SHA1")

  def checksum(algorithm: String): Fold[Array[Byte], String] =
    bytesChecksum(algorithm).contramap[Array[Byte]](a => (a, a.length))

  def bytesChecksum(algorithm: String): Fold[Bytes, String] =
    bytesMessageDigest(algorithm).map(_.map("%02X".format(_)).mkString.toLowerCase)

  def bytesMessageDigest(algorithm: String): Fold[Bytes, Array[Byte]] = new Fold[Bytes, Array[Byte]] {
    type S = MessageDigest
    def start = MessageDigest.getInstance(algorithm)
    def fold = (md, bytes) => { md.update(bytes._1, 0, bytes._2); md }
    def end(s: S) = s.digest
  }
}
