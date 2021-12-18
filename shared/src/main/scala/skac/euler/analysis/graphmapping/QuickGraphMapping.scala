package skac.euler.analysis.graphmapping

import cats.Monad
import cats.implicits._
import skac.euler._
import skac.euler.analysis.BiMap

object QuickGraphMapping {
  type NodeMap = BiMap[NodeIDDesignator, NodeIDDesignator]
  type EdgeMap = BiMap[EdgeIDDesignator, EdgeIDDesignator]

  def empty[UND, UED, DND, DED, UG <: Graph[UND, UED],
    DG <: GraphView[DND, DED, M], M[_]: Monad](upGraph: UG, downGraph: DG) =
    QuickGraphMapping[UND, UED, DND, DED, UG, DG, M](upGraph, downGraph)
}

/**
 * Mapping between elements of two graph views.
 * @param upGraph
 * @param downGraph
 * @param nodeMap
 * @param edgeMap
 * @tparam UND
 * @tparam UED
 * @tparam DND
 * @tparam DED
 */
case class QuickGraphMapping[UND, UED, DND, DED, UG <: Graph[UND, UED], DG <: GraphView[DND, DED, M], M[_]: Monad]
 (override val upSide: UG,
  override val downSide: DG,
  // up graph nodes -> down graph nodes
  nodeMap: QuickGraphMapping.NodeMap = BiMap.empty[NodeIDDesignator, NodeIDDesignator],
  // up graph edges -> down graph edges
  edgeMap: QuickGraphMapping.EdgeMap = BiMap.empty[EdgeIDDesignator, EdgeIDDesignator])
  extends GraphMapping[QuickGraphMapping[UND, UED, DND, DED, UG, DG,  M], UND, UED, DND, DED, UG, DG, M] {

  override def m: Monad[M] = implicitly[Monad[M]]

  def hasUpNode(downNodeDes: NodeDesignator): M[Boolean] = for {
    // down graph node id designator
    d_id_des <- downSide.idDes(downNodeDes)
  } yield (if (d_id_des.isDefined) nodeMap.rightMap.contains(d_id_des.get) else false)

  override def mapDown(upNodeDes: NodeDesignator) = nodeMap.r(upSide.idDes(upNodeDes).get)

  override def mapDown(upEdgeDes: EdgeDesignator): EdgeDesignator = edgeMap.r(upSide.idDes(upEdgeDes).get)
}


