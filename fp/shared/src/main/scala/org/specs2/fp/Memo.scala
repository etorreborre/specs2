package org.specs2.fp

sealed abstract class Memo[@specialized(Int) K, @specialized(Int, Long, Double) V] {
  def apply(z: K => V): K => V
}


object Memo {
  def memo[@specialized(Int) K, @specialized(Int, Long, Double) V](f: (K => V) => K => V): Memo[K, V] = new Memo[K, V] {
    def apply(z: K => V) = f(z)
  }

  def nilMemo[@specialized(Int) K, @specialized(Int, Long, Double) V]: Memo[K, V] = memo[K, V](z => z)

  def mutableMapMemo[K, V](a: collection.mutable.Map[K, V]): Memo[K, V] =
    memo[K, V](f => k => a.getOrElseUpdate(k, f(k)))

  def mutableHashMapMemo[K, V]: Memo[K, V] =
    mutableMapMemo(new collection.mutable.HashMap[K, V])

  def weakHashMapMemo[K, V]: Memo[K, V] =
    mutableMapMemo(new collection.mutable.WeakHashMap[K, V])

  def immutableMapMemo[K, V](m: Map[K, V]): Memo[K, V] = {
    var a = m

    memo[K, V](f =>
      k => {
        a.getOrElse(k, {
          val v = f(k)
          a = a updated (k, v)
          v
        })
      })
  }

  import collection.immutable.{HashMap, ListMap, TreeMap}

  def immutableHashMapMemo[K, V]: Memo[K, V] = immutableMapMemo(HashMap.empty[K, V])

  def immutableListMapMemo[K, V]: Memo[K, V] = immutableMapMemo(ListMap.empty[K, V])

  def immutableTreeMapMemo[K: scala.Ordering, V]: Memo[K, V] = immutableMapMemo(TreeMap.empty[K, V])
}
