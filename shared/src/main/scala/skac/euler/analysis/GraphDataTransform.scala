package skac.euler.analysis

import cats.data.State
import skac.euler._
import skac.euler.AutoModifiableGraph._
import cats.Id
import skac.euler.analysis.graphmapping.QuickGraphMapping

//import scalaz.State

object GraphDataTransform {
//  type NodeTransFun[SND, SED, TND] = (SND, NodeDesignator, Graph[SND, SED]) => TND
//  type EdgeTransFun[SND, SED, TED] = (SED, EdgeDesignator, Graph[SND, SED]) => TED
}

import GraphDataTransform._

/**
  * Generates graph with the same structure (isomorphic) as a source graph but with different data and of different
 * (potentially) class.
  * @param source
  * @param targetBase
  * @param nodeTrans
  * @tparam SG source graph type
  * @tparam SND source node data type
  * @tparam SED source edge data type
  * @tparam TND target node data type
  * @tparam TED target edge data type
  */
abstract class GraphDataTransform[SG <: Graph[SND, SED], TG <: Graph[TND, TED], SND, SED, TND, TED]
  (implicit m: GraphModifier[TG, TND, TED])
  extends ((SG) => (TG, QuickGraphMapping[TND, TED, SND, SED, TG, SG, Id])) {
  type InitData
  type ThisGraphMapping = QuickGraphMapping[TND, TED, SND, SED, TG, SG, Id]

  def targetBase: TG

  /**
    * Generates node data for a given source node.
    * @param srcNode
    * @param srcGraph
    * @return
    */
  def nodeData(srcNode: NodeInfo[SND], srcGraph: SG, initData: InitData): TND

  /**
    * Generates edge data for a given source edge.
    * @param srcEdge
    * @param srgGraph
    * @return
    */
  def edgeData(srcEdge: EdgeInfo[SED], srgGraph: SG, initData: InitData): TED

  /**
   * This method enables descendant class to calculate initial data before method for generating
   * destination node and edge data are called. This data will be supplied to methods generating
   * node and edge data.
   * @return
   */
  def initData(srcGraph: SG): InitData

  override def apply(source: SG): (TG, ThisGraphMapping) = {
    val init_data = initData(source)
    val init_graph: TG = m.clear(targetBase)

    val (graph1, node_map) = (0 until source.nodeCount).foldLeft(
      (init_graph, BiMap.empty[NodeIDDesignator, NodeIDDesignator])) { case ((g, nm), idx: Int) => {
          val src_node = source.node(idx.i).get
          val new_g = m.addNode(g, nodeData(src_node, source, init_data))
          // id designator of newly added node
          val dst_id_des = new_g.lastNode.get.ID.id
          (new_g, nm + (dst_id_des -> src_node.ID.id))
        }
    }

    val (graph2, edge_map) = (0 until source.edgeCount).foldLeft(
      (graph1, BiMap.empty[EdgeIDDesignator, EdgeIDDesignator])) { case ((g, em), idx) => {
        val src_edge = source.edge(idx.ei).get
        val src_node_id = source.node(src_edge.SrcNode).get.ID.id
        val dst_node_id = source.node(src_edge.DstNode).get.ID.id
        val new_g = m.addEdge(g, edgeData(src_edge, source, init_data), node_map.l(src_node_id), node_map.l(dst_node_id))
        val dst_id_des = new_g.lastEdge.get.ID.eid
        (new_g, em + (dst_id_des -> src_edge.ID.eid))
      }
    }

    (graph2, graphmapping.QuickGraphMapping(graph2, source, node_map, edge_map))
  }

//
////    val mg = ModifiableGraph.gTomg[TG, TND, TED](targetBase)
//    lazy val stateTrans = for {
//      _ <- State[(TG, ThisGraphMapping), Unit] { case g => (m.clear(g), GraphMapping.empty(source, ) ())}
//      // adding nodes
//      _ <- State[(TG, ThisGraphMapping), Unit] { case (g, gm) => {
//        (0 until source.nodeCount).foldLeft((g, BiMap.empty[NodeIDDesignator, NodeDesignator]) {
//          case ((g, gm), idx) => {
//            val src_node = source.node(idx.i).get
//            val new_g = m.addNode(g, nodeData(src_node, source, init_data))
//            (new_g, gm + (src_node.ID.id -> idx.i))
//          }
//        }
//      }
//      res <- State[(TG, ThisGraphMapping), Unit] { case g =>
//        val new_g = (0 until source.edgeCount).foldLeft(g) {
//          (g, idx) => {
//            val src_edge = source.edge(idx.ei).get
//            val src_node_id = source.node(src_edge.SrcNode).get.ID.id
//            val dst_node_id = source.node(src_edge.DstNode).get.ID.id
//            m.addEdge(g, edgeData(src_edge, source, init_data), nodes_map(src_node_id), nodes_map(dst_node_id))
//          }
//        }
//        (new_g, ())
//      }
//    } yield res
//
//    stateTrans.runS(targetBase).value
//  }
}
