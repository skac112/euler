package skac.euler.analysis

import skac.euler._
import skac.euler.General._

import scalaz.State

object GraphDataTransform {
  type NodeTransFun[SND, SED, TND] = (SND, NodeDesignator, Graph[SND, SED]) => TND
  type EdgeTransFun[SND, SED, TED] = (SED, EdgeDesignator, Graph[SND, SED]) => TED
}

import GraphDataTransform._

/**
  * Generates graph with the same structure (isomorphic) as a source graph but with different data.
  * @param source
  * @param targetBase
  * @param nodeTrans
  * @tparam SND
  * @tparam SED
  * @tparam TND
  * @tparam TED
  */
class GraphDataTransform[SND, SED, TND, TED](source: Graph[SND, SED],
                                             targetBase: Graph[TND, TED],
                                             nodeTransFun: NodeTransFun[SND, SED, TND],
                                             edgeTransFun: EdgeTransFun[SND, SED, TED]) {

  type NodesMap = Map[NodeIDDesignator, NodeDesignator]

  lazy val stateTrans = for {
    _ <- State[Graph[TND, TED], Unit] {case g => (g.clear, ())}
    // adding nodes
    nodes_map <- State[Graph[TND, TED], NodesMap] { case g => {
      (1 to source.nodeCount).foldLeft((g, Map[NodeIDDesignator, NodeDesignator]())) {
        case ((g, map), idx) => {
          val src_node = source.node(idx.i).get
          val src_node_data = src_node.Data
          g.addNode(nodeTransFun(src_node_data, idx.i, source))
          (g, map + (src_node.ID.id -> idx.i))
        }
      }
    }}
    res <- State[Graph[TND, TED], Unit] { case g =>
      val new_g = (1 to source.edgeCount).foldLeft(g) {
        (g, idx) => {
          val src_edge = source.edge(idx.ei).get
          val src_edge_data = src_edge.Data
          val src_node_id = source.node(src_edge.SrcNode).get.ID.id
          val dst_node_id = source.node(src_edge.DstNode).get.ID.id
          g.addEdge(edgeTransFun(src_edge_data, idx.ei, source), nodes_map(src_node_id), nodes_map(dst_node_id))
        }}
      (new_g, ())
    }
  } yield res

  def transform = stateTrans(targetBase)._1
}
