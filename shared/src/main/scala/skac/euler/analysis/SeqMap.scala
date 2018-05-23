package skac.euler.analysis

object SeqMap {
  def empty[K, V] = new SeqMap[K, V]();
}

/**
  * Map which is also a sequence.
  * @tparam K
  * @tparam V
  */
case class SeqMap[K, V](map: Map[K, V], seq: Seq[(K, V)]) {
  def this() = this(Map.empty, Seq.empty)
  def addToHead(k: K, v: V) = new SeqMap(map + (k -> v), (k, v) +: seq)
  def addToTail(k: K, v: V) = new SeqMap(map + (k -> v), seq :+ (k, v))
  def get(k: K): Option[V] = map.get(k)
  def updated(k: K, v: V) = new SeqMap(map.updated(k, v), {
    val idx = seq.indexWhere(_._1 == k)
    seq.updated(idx, (k, v))
  })
  def isEmpty = seq.isEmpty
  def head: (K, V) = seq.head
  def tail = new SeqMap(map - seq.head._1, seq.tail)
}
