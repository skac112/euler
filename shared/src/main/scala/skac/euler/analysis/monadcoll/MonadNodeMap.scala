package skac.euler.analysis.monadcoll

import cats.Monad
import cats.implicits._
import skac.euler.{GraphView, NodeDataDesignator, NodeDesignator, NodeIDDesignator, NodeIdxDesignator}

object MonadNodeMap {
//  def def empty[S, M[_]: Monad] = MonadNodeMap()
}

case class MonadNodeMap[S, M[_]: Monad](graphView: GraphView[_, _, M], override val baseMap: Map[NodeDesignator, S] = Map.empty[NodeDesignator, S]) extends MonadMap[NodeDesignator, S, M, MonadNodeMap[S, M]] {
  /**
   * Obtains (packed in a monad) a "comparable" value for given key.
   */
  override def compValueM(nodeDes: NodeDesignator): M[Any] = graphView.node(nodeDes) flatMap pure

  override def newInstance(map: Map[NodeDesignator, S]): MonadNodeMap[S, M] = MonadNodeMap(graphView, map)

  override def canBeEqual(k1: NodeDesignator, k2: NodeDesignator): Boolean = (k1, k2) match {
    case (NodeIdxDesignator(idx1), NodeIdxDesignator(idx2)) => idx1 == idx2
    case (NodeIDDesignator(id1), NodeIDDesignator(id2)) => id1 == id2
    case (NodeDataDesignator(data1), NodeDataDesignator(data2)) => data1 == data2
    case _ => true
  }
}
