package org.specs2
package collection

/**
 * Simple BiMap implementation copied from:
 *   http://stackoverflow.com/questions/9850786/is-there-such-a-thing-as-bidirectional-maps-in-scala
 */
private[specs2]
trait BiMap[K, V] {
  def keys: Seq[K]
  def values: Seq[V]

  /** @return the value corresponding to a key */
  def fromKey(k: K): Option[V]
  /** @return true if the map contains the key k */
  def containsKey(k: K) = fromKey(k).isDefined

  /** @return the key corresponding to a value */
  def fromValue(v: V): Option[K]
  /** @return true if the map contains the value v */
  def containsValue(v: V) = fromValue(v).isDefined
}

private[specs2]
object BiMap {
  implicit def keySemiEntry[K](key: K): SemiEntry[K] = new SemiEntry[K] {
    def k = key
  }

  implicit def fromSeq[K, V](s: Seq[BiMapEntry[K, V]]) = new BiMap[K, V] {
    lazy val keys: Seq[K]   = s.map(_.key)
    lazy val values: Seq[V] = s.map(_.value)

    def fromKey(k: K): Option[V]   = s.find(_.key == k).map(_.value)
    def fromValue(v: V): Option[K] = s.find(_.value == v).map(_.key)
  }
}

trait BiMapEntry[K, V] {
  def key:K
  def value:V
}

trait SemiEntry[K] {
  def k: K
  def <->[V](v: V): BiMapEntry[K, V] = new BiMapEntry[K,  V]() { val key = k; val value = v}
}

