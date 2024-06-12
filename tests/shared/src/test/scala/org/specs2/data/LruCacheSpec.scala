package org.specs2
package data

import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.specs2.time.*
import org.specs2.fp.syntax.*
import ProcessedStatus.*

class LruCacheSpec extends Specification with ScalaCheck:
  def is = s2"""

  A LRU cache can be used to store elements and evict them when they have been unused for a long time
    A status is returned to know if an element has already been seen before $e1
    The cache can not contain more than a fixed number of elements $e2
    The oldest elements are always evicted first $e3

  """

  def e1 =
    val cache = LruCache[Int](maxSize = 3, systemTime = MockSystemTime())
    val operations = cache.register(1) >> cache.register(2) >> cache.register(1)
    val status = operations.unsafeRun
    status === AlreadyProcessed

  def e2 = prop { (n: SmallInt) =>
    val cache = LruCache[Int](maxSize = 3, systemTime = MockSystemTime())
    val operations = (1 to n.value).toList.traverse(i => cache.register(i))
    operations.void.unsafeRun
    cache.size must be_<=(3)
  }.set(minTestsOk = 10)

  def e3 = prop { (n: SmallInt) =>
    val mockSystemTime = MockSystemTime()
    val cache = LruCache[Int](maxSize = 3, systemTime = mockSystemTime)
    val operations = (1 to n.value).toList.traverse(i => cache.register(i))
    operations.void.unsafeRun
    cache.oldestTimestamp must be_<(mockSystemTime.nanoTime)
  }.set(minTestsOk = 10)

/** HELPERS */
class MockSystemTime() extends SystemTime:
  private var times: LazyList[Long] = LazyList.from(1).map(_.toLong)

  def nanoTime: Long =
    times match {
      case t #:: ts => times = ts; t
    }

case class SmallInt(value: Int)

object SmallInt {
  given Arbitrary[SmallInt] = Arbitrary {
    arbitrary[Int].map(n => SmallInt((n % 10).abs + 1))
  }
}
