package skac.euler.analysis

object BiMap {
  def empty[L, R] = BiMap[L, R]()
}

/**
 * Bidirectional map.
 * @tparam L
 * @tparam R
 */
case class BiMap[L, R](leftMap: Map[L, R] = Map[L, R]()) {
  lazy val rightMap: Map[R, L] = for ((l, r) <- leftMap) yield (r, l)
  def r(l: L): R = leftMap(l)
  def getR(l: L): Option[R] = leftMap.get(l)
  def l(r: R): L = getL(r).get
  def getL(r: R): Option[L] = rightMap.get(r)
  def +(lr: (L, R)) = this.copy(leftMap + lr)
  def -(l: L) = this.copy(leftMap - l)
}
