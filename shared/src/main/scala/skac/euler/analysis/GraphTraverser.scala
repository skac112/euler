package skac.euler.analysis

import skac.euler.{Graph, _}
import skac.euler.GraphView._

import scala.annotation._
import cats._
import cats.implicits._
import skac.euler.analysis.graphmapping.{QuickGraphMapping, GraphMappingModifier}

object GraphTraverser {
  // type NodeAddFun[S] = (ThisNodeInfo, S, ThisGraphView, ThisTraverser, ResultGraph) => (EPropagation[S], Option[ND2])
  // type EdgeAddFun = (ThisEdgeInfo, S, ThisGraphView, ThisTraverser, ResultGraph) => Option[ED2]
}

/**
 * Traverser which produces a graph. Graph produced is embedded in corresponding mapping. This mapping (wrapped in
 * an M-type monad) is an actual result (of method 'apply').
 * GS - global signal type
 */
abstract class GraphTraverser[ND, ED, ND2, ED2, S, G <: Graph[ND2, ED2], GV <: GraphView[ND, ED, M], M[_] : Monad]
  (implicit m: GraphModifier[G, ND2, ED2])
  extends Traverser[ND, ED, S, QuickGraphMapping[ND2, ED2, ND, ED, G, GV, M],  Map[EdgeDesignator, S], GV, M] {

  import Traverser._
  type ThisGraphMapping = QuickGraphMapping[ND2, ED2, ND, ED, G, GV, M]

  /**
   * Global signal for graph traverser is a map of edges to check. It contains edges which should be processed by
   * edgeAddFun when both incident nodes have their counterparts added in created graph.
   */
  type ThisGlobalSig = Map[EdgeDesignator, S]
//  type ThisRes = (G, ThisGraphMapping)
//  type ResultGraph = Graph[ND2, ED2]
//  type NodeAddFun = (ThisNodeInfo, S, ThisGraphView, ThisTraverser, G) => (EPropagation[S], Option[ND2])
//  type EdgeAddFun = (ThisEdgeInfo, S, ThisGraphView, ThisTraverser, G) => Option[ED2]

  /**
   * Map of nodes added in created graph (node infos from source graphview are
   * keys).
   */
//  var addedNodes: Map[ThisNodeInfo, NodeDesignator] = _

  /**
   *
   */
//  var edgesToCheck: Map[EdgeIDDesignator, S] = _

  val gmm = GraphMappingModifier[ND2, ED2, ND, ED, G, GV, M]

  /**
   * Defines how signal is propagated through edges of a particular node (specified by nInfo) and optionally returns
   * data of new node for produced graph. If new data is specified, new node will be added to produced graph.
   * @param nInfo
   * @param stim
   * @param g
   * @return
   */
  def nodeAddFun(nInfo: ThisNodeInfo, stim: S, gm: ThisGraphMapping): M[(EPropagation[S], Option[ND2])]

  /**
   * Optionally returnd data of new edge for produced graph corresponding to edge in base graphview specified by eInfo.
   * If new data is specified, new edge will be added to produced graph.
   * @param eInfo
   * @param stim
   * @param g
   * @return
   */
  def edgeAddFun(eInfo: ThisEdgeInfo, stim: S, gm: ThisGraphMapping): M[Option[ED2]]

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
                             globalSig: ThisGlobalSig,
                             res: ThisGraphMapping): M[(EPropagation[S], ThisGlobalSig, ThisGraphMapping)] = for {
    node_add_fun_res <- nodeAddFun(nInfo, stim, res)
    e_prop = node_add_fun_res._1
    node_data_o = node_add_fun_res._2
    new_gm_glob_sig <- node_data_o match {
      case Some(node_data) => {
        for {
          new_gm <- gmm.addUpNode(res, nInfo, node_data)
          new_gm_glob_sig_2 <- checkEdgeAdds(nInfo, new_gm, globalSig)
        } yield new_gm_glob_sig_2
      }
      case _ => pure((res, globalSig))
    }
  } yield (e_prop, new_gm_glob_sig._2, new_gm_glob_sig._1)

  private def pureRes(res: ThisGraphMapping): M[ThisGraphMapping] = implicitly[Monad[M]].pure(res)

//  private def pure[V](value: V): M[V] = implicitly[Monad[M]].pure(value)

  /**
    * Function for handling an edge (submitted as EdgeHandleFun in invocation of
    * method 'traverse' from parent class). For each edge to handle it checks if
    * nodes corresponding to incident nodes of this edge have been added to
    * result graph. If both such nodes have been added, an edge add function is
    * called and appropriate edge is added in the result graph. If only one of
    * such nodes has been added, an edge is added to edgesToCheck map. If none
    * of such nodes have been added, it does nothing.
    */
  override def edgeHandleFun(edgeDes: EdgeDesignator,
                             stim: S,
                             globalSig: ThisGlobalSig,
                             res: ThisGraphMapping): M[(ThisGraphMapping, ThisGlobalSig)] = for {
    ei_o <- graphView.edge(edgeDes)
    ei = ei_o.get
    src_node_o <- graphView.node(ei.SrcNode)
    dst_node_o <- graphView.node(ei.DstNode)
    src_node = src_node_o.get
    dst_node = dst_node_o.get
    // checking if src node in down graph view has upper counterpart
    has_src_node <- res.hasUpNode(src_node)
    // checking if dst node in down graph view has upper counterpart
    has_dst_node <- res.hasUpNode(dst_node)
    new_res_and_sig <- if (has_src_node && has_dst_node) {
      for {
        edge_add_res <- edgeAddFun(ei, stim, res)
        edge_add_hndl_res <- handleEdgeAddFun(edge_add_res, ei, res)
      } yield (edge_add_hndl_res, globalSig)
    } else {
      val new_glob_sig =  if (has_src_node || has_dst_node) {
        // global signal is a map of edges to check (edge_des -> signal)
        globalSig + (edgeDes -> stim)
      } else {
        globalSig
      }
      pure((res, new_glob_sig))
    }
  } yield new_res_and_sig


//      if (res.hasUpNode(src_node) && res.hasUpNode(dst_node)) {
//        (for {
//          // both incident nodes in result graph have been added, so an edge can
//          // be added (optionally)
//          edge_add_res <- edgeAddFun(ei, stim, res._1)
//          new_res2 <- handleEdgeAddFun(edge_add_res, ei, res)
//        } yield new_res2)
//    }
//    else {
//      if (addedNodes.contains(src_node) || addedNodes.contains(dst_node)) {
//        // one incident node in result graph is added, so an item in edgesToCheck
//        // map is added - edge can be added later
//        edgesToCheck = edgesToCheck + (eidDes -> stim)
//      }
//      pureRes(res)
//    }
//    }
//  } yield new_res

  def applyWithGraph(initNode: NodeDesignator, initStim: S, initGlobalSig: Map[EdgeDesignator, S], initGraph: G): M[ThisGraphMapping] = {
    val init_res = QuickGraphMapping.empty[ND2, ED2, ND, ED, G, GV, M](initGraph, graphView)
    super.apply(initNode, initStim, initGlobalSig, init_res)
  }

  /**
   * Checks edges to add for given node. It is assumed that for given node
   * (in source graphview) there is a node added in created graph (res).
   * For edges remembered in edgesToCheck map incident with given node the edge
   * add function is executed which can create or not an edge data and an item
   * from edgesToCheck map is removed. If the data is created, appropriate edge
   * is added in the result graph.
   */
  private def checkEdgeAdds(node: ThisNodeInfo, res: ThisGraphMapping, edgesToCheck: ThisGlobalSig): M[(ThisGraphMapping, ThisGlobalSig)] = for {
    incident_edges <- graphView.edges(node) map { edges => edges.map { _.ID.eid }}
    // obtaining all key-value pairs of edges incident to given node contained
    // in edgesToCheck map.
//    edges = incident_edges map {inc_edge =>
//     (inc_edge -> edgesToCheck.get(inc_edge))} filter {kv: (EdgeIDDesignator, Option[S]) => !kv._2.isEmpty} map {
//      kv: (EdgeIDDesignator, Option[S]) => (kv._1, kv._2.get)
//     }
    edges = edgesToCheck.filterKeys(e1 => incident_edges.exists(_ == e1))
    new_res <- edges.toList.foldM(res) { (curr_res: ThisGraphMapping, kv: (EdgeDesignator, S)) => for {
      src_ei <- graphView.edge(kv._1) map { _.get }
      dst_e_data_o <- edgeAddFun(src_ei, kv._2, res)
      curr_new_res <- handleEdgeAddFun(dst_e_data_o, src_ei, curr_res)
    } yield curr_new_res }
    new_edges_to_check = edgesToCheck -- edges.keys
  } yield (new_res, new_edges_to_check)

  private def handleEdgeAddFun(addRes: Option[ED2], srcEdgeInfo: ThisEdgeInfo, res: ThisGraphMapping): M[ThisGraphMapping] = addRes match {
    case Some(edge_data) => for {
      // nodes in graphview
      src_node_o <- graphView.node(srcEdgeInfo.SrcNode)
      src_node = src_node_o.get
      dst_node_o <- graphView.node(srcEdgeInfo.DstNode)
      dst_node = dst_node_o.get
      // nodes in created graph
//      res_src_node = res._1.node(addedNodes(src_node)).get
//      res_dst_node = res._1.node(addedNodes(dst_node)).get
      edge_added <- gmm.addUpEdge(res, srcEdgeInfo.ID.eid, edge_data)
    } yield edge_added
    case _ => pureRes(res)
  }
}
