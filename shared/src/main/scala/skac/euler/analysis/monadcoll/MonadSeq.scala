package skac.euler.analysis.monadcoll

import cats.Monad

abstract class MonadSeq[V, M[_]: Monad, MS <: MonadSeq[V, M, MS]] extends MonadColl[V, M] {
  def baseSeq: Seq[V]
  def newInstance(seq: Seq[V]): MS

  /**
   * Adds value to head position in baseSeq. Assumes that value doesn't exist in seq, otherwise the result is undefined (value can
   * be duplicated due to comparator value).
   * @param kv
   * @return
   */
  def addOnlyToHead(v: V): MS = newInstance(v +: baseSeq)

  /**
   * Adds value to tail position in baseSeq. Assumes that value doesn't exist in seq, otherwise the result is undefined (value can
   * be duplicated due to comparator value).
   * @param kv
   * @return
   */
  def addOnlyToTail(v: V): MS = newInstance(baseSeq :+ v)

}
