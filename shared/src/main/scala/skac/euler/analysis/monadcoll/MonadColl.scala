package skac.euler.analysis.monadcoll

import cats.Monad

abstract class MonadColl[K, M[_]: Monad] {
  /**
   * Obtains (packed in a monad) a "comparable" value for given key.
   */
  def compValueM(k: K): M[Any]

  def canBeEqual(k1: K, k2: K): Boolean

  def pure[V](v: V) = implicitly[Monad[M]].pure(v)
}
