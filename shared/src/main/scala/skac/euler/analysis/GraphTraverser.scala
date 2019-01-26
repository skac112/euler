package skac.euler.analysis

import skac.euler.General._
import skac.euler._
import skac.euler.GraphView._
import scala.annotation._

object GraphTraverser {
  // type NodeAddFun[S] = (ThisNodeInfo, S, ThisGraphView, ThisTraverser, ResultGraph) => (EPropagation[S], Option[ND2])
  // type EdgeAddFun = (ThisEdgeInfo, S, ThisGraphView, ThisTraverser, ResultGraph) => Option[ED2]
}

/**
 * Traverser which produces a graph.
 */
class GraphTraverser[ND, ED, ND2, ED2, S, G <: Graph[ND2, ED2]](graphView: GraphView[ND, ED])
 extends Traverser[ND, ED, S, G](graphView) {
  import Traverser._
//  type ResultGraph = Graph[ND2, ED2]
  type NodeAddFun = (ThisNodeInfo, S, ThisGraphView, ThisTraverser, G) => (EPropagation[S], Option[ND2])
  type EdgeAddFun = (ThisEdgeInfo, S, ThisGraphView, ThisTraverser, G) => Option[ED2]

  /**
   * Map of nodes added in created graph (node infos from source graphview are
   * keys).
   */
  var addedNodes: Map[ThisNodeInfo, NodeDesignator] = _

  /**
   *
   */
  var edgesToCheck: Map[EdgeIDDesignator, S] = _

  def makeGraph(initNode: NodeDesignator,
   initStim: S,
   stimMergeFun: StimMergeFun,
   nodeAddFun: NodeAddFun,
   edgeAddFun: EdgeAddFun,
   initGraph: G): G = {
     // map of nodes added in created graph (node infos from source graphview are keys)
     addedNodes = Map()
     edgesToCheck = Map()
     traverse(initNode, initStim, stimMergeFun, nodeHandleFun(nodeAddFun, edgeAddFun),
      edgeHandleFun(edgeAddFun), initGraph)
  }

  private def nodeHandleFun(nodeAddFun: NodeAddFun, edgeAddFun: EdgeAddFun) =
   (node: ThisNodeInfo, stim: S, graphView: ThisGraphView,
    traverser: ThisTraverser, res: G) => {
      val (e_prop, node_data_o) = nodeAddFun(node, stim, graphView, traverser, res)
      val new_res = node_data_o match {
        case Some(node_data) => {
          val res1 = res.addNode(node_data).asInstanceOf[G]
          // retrieving designator of added node (we can't rely on node data which can be
          // not unique)
          val added_node = res1.node((res.nodeCount).i).get
          addedNodes = addedNodes + (node -> added_node.ID.id)
          checkEdgeAdds(node, edgeAddFun, res1)
        }
        case _ => res
      }
      (e_prop, new_res)
    }

  /**
   * Function for handling an edge (submitted as EdgeHandleFun in invocation of
   * method 'traverse' from parent class). For each edge to handle it checks if
   * nodes corresponding to incident nodes of this edge have been added to
   * result graph. If both such nodes have been added, an edge add function is
   * called and appropriate edge is added in the result graph. If only one of
   * such nodes has been added, an edge is added to edgesToCheck map. If none
   * of such nodes have been added, it does nothing.
   */
  private def edgeHandleFun(edgeAddFun: EdgeAddFun): EdgeHandleFun =
   (edge: EdgeIDDesignator, stim: S, graphView: ThisGraphView,
   traverser: ThisTraverser, res: G) => {
     val ei = graphView.edge(edge).get
     val src_node = graphView.node(ei.SrcNode).get
     val dst_node = graphView.node(ei.DstNode).get
     if (addedNodes.contains(src_node) && addedNodes.contains(dst_node)) {
       // both incident nodes in result graph have been added, so an edge can
       // be added (optionally)
       val edge_add_res = edgeAddFun(ei, stim, graphView, traverser, res)
       handleEdgeAddFun(edge_add_res, ei, res)
     }
     else {
       if (addedNodes.contains(src_node) || addedNodes.contains(dst_node)) {
         // one incident node in result graph is added, so an item in edgesToCheck
         // map is added - edge can be added later
         edgesToCheck = edgesToCheck + (edge -> stim)
       }
       res
     }
   }

  /**
   * Checks edges to add for given node. It is assumed that for given node
   * (in source graphview) there is an node added in created graph (res).
   * For edges remembered in edgesToCheck map incident with given node the edge
   * add function is executed which can create or not an edge data and an item
   * from edgesToCheckMap is removed. If the data is created, appropriate edge
   * is added in the result graph.
   */
  private def checkEdgeAdds(node: ThisNodeInfo, edgeAddFun: EdgeAddFun,
   res: G): G = {
    val incident_edges = graphView.edges(node) map {_.ID.eid}
    // obtaining all key-value pairs of edges incident to given node contained
    // in edgesToCheck map.
    val edges: Set[(EdgeIDDesignator, S)] = incident_edges map {inc_edge =>
     (inc_edge -> edgesToCheck.get(inc_edge))} filter {kv: (EdgeIDDesignator, Option[S]) => !kv._2.isEmpty} map {
      kv: (EdgeIDDesignator, Option[S]) => (kv._1, kv._2.get)
     }

    edges.foldLeft(res) {(curr_res: G, kv: (EdgeIDDesignator, S)) => {
      val ei = graphView.edge(kv._1).get
      val edge_add_res = edgeAddFun(ei, kv._2, graphView, this, res)
      handleEdgeAddFun(edge_add_res, ei, curr_res)
    }}
  }

  private def handleEdgeAddFun(addRes: Option[ED2], edgeInfo: ThisEdgeInfo,
   res: G): G = addRes match {
    case Some(edge_data) => {
      // nodes in graphview
      val src_node = graphView.node(edgeInfo.SrcNode).get
      val dst_node = graphView.node(edgeInfo.DstNode).get
      // nodes in created graph
      val res_src_node = res.node(addedNodes(src_node)).get
      val res_dst_node = res.node(addedNodes(dst_node)).get
      res.addEdge(edge_data, res_src_node, res_dst_node).asInstanceOf[G]
    }
    case _ => res
  }
}
