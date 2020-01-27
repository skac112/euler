package skac.euler.analysis

import cats.data.State
import skac.euler._
import skac.euler.AutoModifiableGraph._

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
abstract class GraphDataTransform[SG <: Graph[SND, SED], TG <: Graph[TND, TED], SND, SED, TND, TED](implicit m: GraphModifier[TG, TND, TED]) extends ((SG) => TG) {
  type NodesMap = Map[NodeIDDesignator, NodeDesignator]
  type InitData

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

  override def apply(source: SG): TG = {
    val init_data = initData(source)
//    val mg = ModifiableGraph.gTomg[TG, TND, TED](targetBase)
    lazy val stateTrans = for {
      _ <- State[TG, Unit] { case g => (m.clear(g), ())}
      // adding nodes
      nodes_map <- State[TG, NodesMap] { case g => {
        (0 until source.nodeCount).foldLeft((g, Map[NodeIDDesignator, NodeDesignator]())) {
          case ((g, map), idx) => {
            val src_node = source.node(idx.i).get
            m.addNode(g, nodeData(src_node, source, init_data))
            (g, map + (src_node.ID.id -> idx.i))
          }
        }
      }
      }
      res <- State[TG, Unit] { case g =>
        val new_g = (0 until source.edgeCount).foldLeft(g) {
          (g, idx) => {
            val src_edge = source.edge(idx.ei).get
            val src_node_id = source.node(src_edge.SrcNode).get.ID.id
            val dst_node_id = source.node(src_edge.DstNode).get.ID.id
            m.addEdge(g, edgeData(src_edge, source, init_data), nodes_map(src_node_id), nodes_map(dst_node_id))
          }
        }
        (new_g, ())
      }
    } yield res

    stateTrans.runS(targetBase).value
  }
}
