package skac.euler.analysis.monadcoll

import cats.FlatMap.ops.toAllFlatMapOps
import cats.Monad
import skac.euler.{EdgeDataDesignator, EdgeDesignator, EdgeIDDesignator, EdgeIdxDesignator, GraphView, NodeDataDesignator, NodeDesignator, NodeIDDesignator, NodeIdxDesignator}

case class MonadEdgeSet[M[_]: Monad](
  graphView: GraphView[_, _, M],
  override val baseSet: Set[EdgeDesignator] = Set.empty)
  extends MonadSet[EdgeDesignator, M, MonadEdgeSet[M]] {

  override def newInstance(set: Set[EdgeDesignator]) = MonadEdgeSet[M](graphView, set)

  /**
   * Obtains (packed in a monad) a "comparable" value for given key.
   */
  override def compValueM(edgeDes: EdgeDesignator): M[Any] = graphView.edge(edgeDes) flatMap pure

  override def canBeEqual(edgeDes1: EdgeDesignator, edgeDes2: EdgeDesignator): Boolean = (edgeDes1, edgeDes2) match {
    case (EdgeIdxDesignator(idx1), EdgeIdxDesignator(idx2)) => idx1 == idx2
    case (EdgeIDDesignator(id1), EdgeIDDesignator(id2)) => id1 == id2
    case (EdgeDataDesignator(data1), EdgeDataDesignator(data2)) => data1 == data2
    case _ => true
  }
}

