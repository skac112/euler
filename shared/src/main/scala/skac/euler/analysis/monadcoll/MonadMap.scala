package skac.euler.analysis.monadcoll

import cats.Monad
import cats.implicits.{catsSyntaxFlatMapIdOps, _}

/**
 * Kind of map differing from typical map by two properties:
 * -
 * @param monad$M$0
 * @tparam K
 * @tparam V
 * @tparam M
 * @tparam MM
 */
abstract class MonadMap[K, V, M[_]: Monad, MM <: MonadMap[K, V, M, MM]] extends MonadColl[K, M] {
  def baseMap: Map[K, V]
  def newInstance(map: Map[K, V]): MM
  def comparableSubMap(k: K): Map[K, V] = baseMap filter {kv: (K, V) => canBeEqual(k, kv._1)}

  private def findByCompM(k: K, compVal: Any): M[Option[(K, V)]] = {
    // obtaining submap of baseMap containing only possibly equal keys (by comparator value equality)
    val submap = comparableSubMap(k)
    submap.tailRecM[M, Option[(K, V)]] { current: Map[K, V] => if (!current.isEmpty) {
        if (canBeEqual(current.head._1, k)) {
          for {
            curr_comp_val <- compValueM(current.head._1)
          } yield if (curr_comp_val == compVal) Right(Some(current.head)) else Left(current.tail)
        }
        else {
          pure (Left(current.tail))
        }
    } else {
      pure(Right(None))
    }}
  }
  /**
   * Determines if map can possibly contain given key. Returning true means that either:
   * - key exists in a baseMap (in a default meaning of equality, not taking comparator value into account)
   * - key doesn't exists in a baseMap but can be equal to other key by comparator value (i. e. it's comparator value
   * can be equal to some key's in a baseMap comparator value)
   */
  def canHaveKey(k: K): Boolean = baseMap exists { kv => canBeEqual(kv._1, k) }

  def findPair(k: K): M[Option[(K, V)]] = baseMap.get(k) match {
    case Some(v) => pure(Some(k, v))
    // There are no key equal to k in baseMap in a normal sense, but still there can be some key equal by comparator
    // value. Before comparator value is computed even for k, a checking is provided to determine if any key can
    // be potentially equal (by comparator value).
    case None => if (canHaveKey(k)) {
        for {
          comp_value <- compValueM(k)
          found_o <- findByCompM(k, comp_value)
        } yield found_o
      }
      else {
        pure(None)
      }
  }

  /**
   * Adds or updates value for given key.
   * @param kv
   * @return
   */
  def +(kv: (K, V)): M[MM] = for {
    pair_o <- findPair(kv._1)
    pair = pair_o match {
      case Some((k, v)) => (k, kv._2)
      case None => kv
    }
    new_map = baseMap + pair
  } yield newInstance(new_map)

  def -(k: K): M[MM] = for {
    pair_o <- findPair(k)
    new_map = pair_o match {
      case Some((k, v)) => baseMap - k
      case None => baseMap
    }
  } yield newInstance(new_map)

  /**
   * Adds keyvalue pair to map. Assumes that key doesn't exist in map, otherwise the result is undefined (key can
   * be duplicated due to comparator value).
   * @param kv
   * @return
   */
  def addOnly(kv: (K, V)): MM = newInstance(baseMap + kv)

  def updated(k: K, v: V): M[MM] = this.+((k, v))
}
