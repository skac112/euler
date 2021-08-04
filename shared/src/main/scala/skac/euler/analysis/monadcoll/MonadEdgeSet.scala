package skac.euler.analysis.monadcoll

import cats.Monad
import skac.euler.{EdgeDesignator, GraphView}

case class MonadEdgeSet[M[_]: Monad](
  graphView: GraphView[_, _, M],
  override val baseSet: Set[EdgeDesignator] = Set.empty)

