package skac.euler.analysis.graphmapping

import cats.Monad
import cats.implicits._
import skac.euler.{Graph, GraphModifier, GraphView, NodeDesignator}
import skac.euler._

/**
 * Enables modifying up graph and corresponding mapping.
 * @param monad$M$0
 * @param m
 * @tparam UND
 * @tparam UED
 * @tparam DND
 * @tparam DED
 * @tparam UG
 * @tparam DG
 * @tparam M
 */
case class GraphMappingModifier[UND, UED, DND, DED, UG <: Graph[UND, UED], DG <: GraphView[DND, DED, M], M[_]: Monad]
  (implicit m: GraphModifier[UG, UND, UED]) {
  type ThisGraphMapping = QuickGraphMapping[UND, UED, DND, DED, UG, DG, M]

  /**
   * Adds node to up graph matched with down graph view node and updates corresponding mapping. Any kind of node
   * designator can be used to designate down graph view node.
   *
   * @param gm
   * @param downNodeDes
   * @param upNodeData
   * @return
   */
  def addUpNode(gm: ThisGraphMapping, downNodeDes: NodeDesignator, upNodeData: UND): M[ThisGraphMapping] = {
    // adding node to up graph
    val new_up_g = m.addNode(gm.upSide, upNodeData)
    // id designator of added node
    val up_des: NodeIDDesignator = new_up_g.lastNode.get.ID.id
    for {
      down_des_o <- gm.downSide.idDes(downNodeDes)
      new_node_map = gm.nodeMap + (up_des -> down_des_o.get)
    } yield (gm.copy(upSide = new_up_g, nodeMap = new_node_map))
  }

  //  def addUpEdge(gm: ThisGraphMapping, downEdgeDes: EdgeDesignator, upEdgeData: UED): M[ThisGraphMapping] = ???

  def addUpEdge(gm: ThisGraphMapping, downEdgeDes: EdgeDesignator, upEdgeData: UED): M[ThisGraphMapping] = for {
    d_edge_id_des_o <- gm.downSide.idDes(downEdgeDes)
    d_edge_id_des = d_edge_id_des_o.get
    d_edge_o <- gm.downSide.edge(d_edge_id_des)
    d_edge = d_edge_o.get
    d_src_node_o <- gm.downSide.idDes(d_edge.SrcNode)
    d_dst_node_o <- gm.downSide.idDes(d_edge.DstNode)
    d_src_node = d_src_node_o.get
    d_dst_node = d_dst_node_o.get
    u_src_node = gm.nodeMap.getL(d_src_node).get
    u_dst_node = gm.nodeMap.getL(d_dst_node).get
    new_up_g = m.addEdge(gm.upSide, upEdgeData, u_src_node, u_dst_node)
    up_edge_des = new_up_g.lastEdge.get.ID.eid
    new_edge_map = gm.edgeMap + (up_edge_des -> d_edge_id_des)
  } yield (gm.copy(upSide = new_up_g, edgeMap = new_edge_map))
}