package skac.euler.analysis.monadcoll

import cats._
import cats.Monad
import cats.implicits.{catsSyntaxFlatMapIdOps, _}
import cats.implicits._

/**
 *
 * @param monad$M$0
 * @tparam K
 * @tparam V
 * @tparam M
 * @tparam MS
 */
abstract class MonadSet[V, M[_]: Monad, MS <: MonadSet[V, M, MS]] extends MonadColl[V, M] {
  this: MS =>
  def baseSet: Set[V]
  def newInstance(set: Set[V]): MS
  def comparableSubset(v: V): Set[V] = baseSet filter { curr: (V) => canBeEqual(curr, v)}

  private def findByCompM(v: V, compVal: Any): M[Option[V]] = {
    // obtaining subset of baseSet containing only possibly equal values (by comparator value equality)
    val subset = comparableSubset(v)
    subset.tailRecM[M, Option[V]] { current: Set[V] => if (!current.isEmpty) {
        if (canBeEqual(current.head, v)) {
          for {
            curr_comp_val <- compValueM(current.head)
          } yield if (curr_comp_val == compVal) Right(Some(current.head)) else Left(current.tail)
        }
        else {
          pure(Left(current.tail))
        }
    } else {
      pure(Right(None))
    }}
  }

  def apply(v: V): M[Boolean] = for {
    f <- find(v)
  } yield (!f.isEmpty)

  def find(v: V): M[Option[V]] = if (baseSet(v)) {
      pure(Some(v))
    }
    else {
      if (canHaveVal(v)) {
        for {
          comp_value <- compValueM(v)
          found_o <- findByCompM(v, comp_value)
        } yield found_o
      }
      else {
        pure(None)
      }
    }

  /**
   * Determines if set can possibly contain given key. Returning true means that either:
   * - key exists in a baseSet (in a default meaning of equality, not taking comparator value into account)
   * - key doesn't exists in a baseSet but can be equal to other key by comparator value (i. e. it's comparator value
   * can be equal to some key's in a baseSet comparator value)
   */
  def canHaveVal(v: V): Boolean = baseSet exists { curr => canBeEqual(curr, v) }

  /**
   * Adds value if it doesn't exist taking possible equality by comparator value.
   * @param kv
   * @return
   */
  def +(v: V): M[MS] = for {
    found_o <- find(v)
    res = found_o match {
      case Some(v) => newInstance(baseSet + v)
      case None => this
    }
  } yield res

  /**
   * Removes value if it exists taking possible equality by comparator value.
   * @param kv
   * @return
   */
  def -(v: V): M[MS] = for {
    found_o <- find(v)
    res = found_o match {
      case Some(v) => newInstance(baseSet - v)
      case None => this
    }
  } yield res

//  private def addToSet(set: Set[V], v: V): M[Set[V]] = for {
//    found_o <- find(v)
//    res = found_o match {
//      case Some(v) => baseSet + v
//      case None => baseSet
//    }
//  } yield res

  def ++(values: Iterable[V]): M[MS] = {
    val start = newInstance(baseSet)
    values.toList.foldM(start){ (curr_set, value) => curr_set + value }
  }
}
