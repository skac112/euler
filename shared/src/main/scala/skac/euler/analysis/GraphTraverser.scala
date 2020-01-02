package skac.euler.analysis

import skac.euler._
import skac.euler.GraphView._
import scala.annotation._
import cats._
import cats.implicits._

object GraphTraverser {
  // type NodeAddFun[S] = (ThisNodeInfo, S, ThisGraphView, ThisTraverser, ResultGraph) => (EPropagation[S], Option[ND2])
  // type EdgeAddFun = (ThisEdgeInfo, S, ThisGraphView, ThisTraverser, ResultGraph) => Option[ED2]
}

/**
 * Traverser which produces a graph.
 */
abstract class GraphTraverser[ND, ED, ND2, ED2, S, G <: ModifiableGraph[G, ND2, ED2], M[_] : Monad]
  extends Traverser[ND, ED, S, G, M] {
  import Traverser._
//  type ResultGraph = Graph[ND2, ED2]
//  type NodeAddFun = (ThisNodeInfo, S, ThisGraphView, ThisTraverser, G) => (EPropagation[S], Option[ND2])
//  type EdgeAddFun = (ThisEdgeInfo, S, ThisGraphView, ThisTraverser, G) => Option[ED2]

  /**
   * Map of nodes added in created graph (node infos from source graphview are
   * keys).
   */
  var addedNodes: Map[ThisNodeInfo, NodeDesignator] = _

  /**
   *
   */
  var edgesToCheck: Map[EdgeIDDesignator, S] = _

  def nodeAddFun(nInfo: ThisNodeInfo, stim: S, g: G): M[(EPropagation[S], Option[ND2])]

  def edgeAddFun(eInfo: ThisEdgeInfo, stim: S, g: G): M[Option[ED2]]

  /**
    * Defines how signal in node propagates to edges and modifies current result.
    *
    * @param nInfo
    * @param stim
    * @param gv
    * @param res
    * @return
    */
  override def nodeHandleFun(nInfo: ThisNodeInfo,
                             stim: S,
                             res: G): M[(EPropagation[S], G)] = for {
    node_add_fun_res <- nodeAddFun(nInfo, stim, res)
    e_prop = node_add_fun_res._1
    node_data_o = node_add_fun_res._2
    new_res <- node_data_o match {
      case Some(node_data) => {
        val res1 = res.addNode(node_data)
        // retrieving designator of added node (we can't rely on node data which can be
        // not unique)
        val added_node = res1.node(res.nodeCount.i).get
        addedNodes = addedNodes + (nInfo -> added_node.ID.id)
        checkEdgeAdds(nInfo, res1)
      }
      case _ => pureG(res)
    }
  } yield (e_prop, new_res)

  private def pureG(g: G): M[G] = implicitly[Monad[M]].pure(g)

  /**
    * Function for handling an edge (submitted as EdgeHandleFun in invocation of
    * method 'traverse' from parent class). For each edge to handle it checks if
    * nodes corresponding to incident nodes of this edge have been added to
    * result graph. If both such nodes have been added, an edge add function is
    * called and appropriate edge is added in the result graph. If only one of
    * such nodes has been added, an edge is added to edgesToCheck map. If none
    * of such nodes have been added, it does nothing.
    */
  override def edgeHandleFun(eidDes: EdgeIDDesignator,
                             stim: S,
                             res: G): M[G] = for {
    ei_o <- graphView.edge(eidDes)
    ei = ei_o.get
    src_node_o <- graphView.node(ei.SrcNode)
    dst_node_o <- graphView.node(ei.DstNode)
    src_node = src_node_o.get
    dst_node = dst_node_o.get
    new_res <- if (addedNodes.contains(src_node) && addedNodes.contains(dst_node)) {
      for {
        // both incident nodes in result graph have been added, so an edge can
        // be added (optionally)
        edge_add_res <- edgeAddFun(ei, stim, res)
        new_res2 <- handleEdgeAddFun(edge_add_res, ei, res)
      } yield new_res2
    }
    else {
      if (addedNodes.contains(src_node) || addedNodes.contains(dst_node)) {
        // one incident node in result graph is added, so an item in edgesToCheck
        // map is added - edge can be added later
        edgesToCheck = edgesToCheck + (eidDes -> stim)
      }
      pureG(res)
    }
  } yield res

  override def apply(initNode: NodeDesignator, initStim: S, initGraph: G): M[G] = {
    addedNodes = Map()
    edgesToCheck = Map()
    super.apply(initNode, initStim, initGraph)
  }

  /**
   * Checks edges to add for given node. It is assumed that for given node
   * (in source graphview) there is an node added in created graph (res).
   * For edges remembered in edgesToCheck map incident with given node the edge
   * add function is executed which can create or not an edge data and an item
   * from edgesToCheckMap is removed. If the data is created, appropriate edge
   * is added in the result graph.
   */
  private def checkEdgeAdds(node: ThisNodeInfo, res: G): M[G] = for {
    incident_edges <- graphView.edges(node) map { edges => edges.map { _.ID.eid }}
    // obtaining all key-value pairs of edges incident to given node contained
    // in edgesToCheck map.
    edges = incident_edges map {inc_edge =>
     (inc_edge -> edgesToCheck.get(inc_edge))} filter {kv: (EdgeIDDesignator, Option[S]) => !kv._2.isEmpty} map {
      kv: (EdgeIDDesignator, Option[S]) => (kv._1, kv._2.get)
     }
    new_res <- edges.toList.foldM(res) { (curr_res: G, kv: (EdgeIDDesignator, S)) => for {
      ei <- graphView.edge(kv._1) map { _.get }
      edge_add_res <- edgeAddFun(ei, kv._2, res)
      curr_new_res <- handleEdgeAddFun(edge_add_res, ei, curr_res)
    } yield curr_new_res }
  } yield new_res

  private def handleEdgeAddFun(addRes: Option[ED2], edgeInfo: ThisEdgeInfo, res: G): M[G] = addRes match {
    case Some(edge_data) => for {
      // nodes in graphview
      src_node_o <- graphView.node(edgeInfo.SrcNode)
      src_node = src_node_o.get
      dst_node_o <- graphView.node(edgeInfo.DstNode)
      dst_node = dst_node_o.get
      // nodes in created graph
      res_src_node = res.node(addedNodes(src_node)).get
      res_dst_node = res.node(addedNodes(dst_node)).get
    } yield res.addEdge(edge_data, res_src_node, res_dst_node)
    case _ => implicitly[Monad[M]].pure(res)
  }
}
