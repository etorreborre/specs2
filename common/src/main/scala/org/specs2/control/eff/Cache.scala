package org.specs2.control.eff

import java.util
import java.util.concurrent._

/**
 * This cache is used to memoize values for the Memoized effect
 */
trait Cache {

  type C <: Cache

  /**
   * store a value for a given key, subsequent calls to memo will return the same value
   */
  def memo[V](key: AnyRef, value: =>V): V

  /**
   * put a value for a given key and override the previous value if present
   */
  def put[V](key: AnyRef, value: V): V

  /**
   * get a value for a given key
   */
  def get[V](key: AnyRef): Option[V]

  /**
   * remove the given key
   */
  def reset(key: AnyRef): C

}

/**
 * type class for effects which can be cached
 * in a SequenceCache
 */
trait SequenceCached[M[_]] {
  def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, tx: =>M[X]): M[X]
}

case class ConcurrentHashMapCache(map: ConcurrentHashMap[AnyRef, Evaluated[Any]] = new ConcurrentHashMap[AnyRef, Evaluated[Any]]) extends Cache {

  type C = Cache

  def memo[V](key: AnyRef, value: =>V): V = {
    lazy val v = value
    if (map.putIfAbsent(key, Memoized(v)) == null) v
    else map.get(key).value.asInstanceOf[V]
  }

  def put[V](key: AnyRef, value: V): V = {
    val v = Now(value)
    map.put(key, v)
    Option(map.get(key)).getOrElse(v).value.asInstanceOf[V]
  }

  def get[V](key: AnyRef): Option[V] =
    Option(map.get(key)).map(_.value.asInstanceOf[V])

  def reset(key: AnyRef) = {
    map.remove(key)
    this
  }
}

case class ConcurrentWeakIdentityHashMapCache(
                                               map: ConcurrentWeakIdentityHashMap[AnyRef, Evaluated[Any]] = new ConcurrentWeakIdentityHashMap[AnyRef, Evaluated[Any]]) extends Cache {

  type C = Cache

  def memo[V](key: AnyRef, value: =>V): V = {
    lazy val v = value
    if (map.putIfAbsent(key.asInstanceOf[AnyRef], Memoized(v)) == null) v
    else map.get(key.asInstanceOf[AnyRef]).value.asInstanceOf[V]
  }

  def put[V](key: AnyRef, value: V): V =
    map.put(key, Now(value)).value.asInstanceOf[V]

  def get[V](key: AnyRef): Option[V] =
    Option(map.get(key)).map(_.value.asInstanceOf[V])

  def reset(key: AnyRef) = {
    map.remove(key.hashCode)
    this
  }

}

/////

/*
 * Copyright Terracotta, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.lang.ref.ReferenceQueue
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap

/**
 * @author Alex Snaps
 */
class ConcurrentWeakIdentityHashMap[K, V] extends ConcurrentMap[K, V] {

  private val map = new ConcurrentHashMap[WeakReference[K], V]
  private val queue = new ReferenceQueue[K]

  override def putIfAbsent(key: K, value: V): V = {
    purgeKeys
    map.putIfAbsent(newKey(key), value)
  }

  def get(key: Object): V =  {
    purgeKeys
    map.get(new WeakReference[Object](key, null))
  }

  def clear(): Unit = {
    purgeKeys
    map.clear
  }

  def containsKey(key: Any): Boolean = {
    purgeKeys
    map.containsKey(new WeakReference[K](key.asInstanceOf[K], null))
  }

  def containsValue(value: Object): Boolean = {
    purgeKeys
    map.containsValue(value)
  }

  def isEmpty: Boolean = {
    purgeKeys
    map.isEmpty
  }

  def remove(key: Any): V = {
    purgeKeys
    map.remove(new WeakReference[K](key.asInstanceOf[K], null))
  }

  def size: Int = {
    purgeKeys
    map.size
  }

  def put(key: K, value: V): V =  {
    purgeKeys
    map.put(newKey(key), value)
  }

  def keySet(): java.util.Set[K] = {
    new util.AbstractSet[K] {
      def iterator: java.util.Iterator[K] = {
        purgeKeys
        new WeakSafeIterator[K, WeakReference[K]](map.keySet.iterator) {
          def extract(u: WeakReference[K]): K = u.get
        }
      }

      override def contains(o: Object): Boolean = ConcurrentWeakIdentityHashMap.this.containsKey(o)
      def size = map.size
    }

  }

  def entrySet(): java.util.Set[java.util.Map.Entry[K,V]] =     new util.AbstractSet[java.util.Map.Entry[K,V]] {
    def iterator: java.util.Iterator[java.util.Map.Entry[K,V]] = {
      purgeKeys
      new WeakSafeIterator[java.util.Map.Entry[K,V], java.util.Map.Entry[WeakReference[K], V]](map.entrySet.iterator) {
        def extract(u: java.util.Map.Entry[WeakReference[K], V]): java.util.Map.Entry[K,V] = {
          val key = u.getKey.get
          if (key == null) null
          else new java.util.AbstractMap.SimpleEntry(key, u.getValue)
        }
      }
    }
    def size = map.size
  }

  def putAll(m: java.util.Map[_ <: K, _ <: V]): Unit = {
    purgeKeys
    import scala.collection.JavaConverters._
    m.entrySet.asScala.foreach(e => map.put(newKey(e.getKey), e.getValue))
  }

  def values: java.util.Collection[V] = {
    purgeKeys
    map.values
  }


  private def purgeKeys(): Unit = {
    var reference = queue.poll
    while (reference != null) {
      reference = queue.poll
      map.remove(reference)
    }
  }

  private def newKey(key: K): WeakReference[K] =  {
    new WeakReference[K](key, queue)
  }

  private class WeakReference[T](referent: T, queue: ReferenceQueue[T]) extends java.lang.ref.WeakReference[T](referent, queue) {

    override def hashCode: Int = System.identityHashCode(referent)

    override def equals(a: Any): Boolean = {
      a != null && a.getClass == this.getClass &&
        (this == a || this.get == a.asInstanceOf[WeakReference[T]].get)
    }

  }

  private abstract class WeakSafeIterator[T, U](weakIterator: java.util.Iterator[U]) extends java.util.Iterator[T] {
    advance
    private var strongNext: T = null.asInstanceOf[T]

    def advance(): Unit  = {
      while (weakIterator.hasNext) {
        val nextU = weakIterator.next
        strongNext = extract(nextU)
        if (strongNext != null) return
      }
      strongNext = null.asInstanceOf[T]
    }

    def hasNext: Boolean = strongNext != null

    def next: T = {
      val next = strongNext
      advance
      next
    }

    override def remove = throw new UnsupportedOperationException()

    def extract(u: U): T
  }

}

