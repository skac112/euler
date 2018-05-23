package skac.euler.analysis

import skac.euler.General._
import skac.euler._
import skac.euler.GraphView._
import scala.annotation._

object Traverser {
  type NSet = Set[NodeIDDesignator]
  type ESet = Set[EdgeIDDesignator]
  type ESignal[S] = (EdgeIDDesignator, S)
  type NSignal[S] = (NodeIDDesignator, S)
  type NSList[S] = Seq[NSignal[S]]
  type ESList[S] = Seq[ESignal[S]]
  type Signals[S] = SeqMap[NodeIDDesignator, S]
  case class EPropagation[S](toHead: ESList[S], toTail: ESList[S])
  case class NPropagation[S](toHead: NSList[S], toTail: NSList[S])
}

/**
 * Traverses graph view from start node or many start nodes using signals. Signals are propagated from
 * node through incident edges to other (adjacent) nodes. There are two types of signals -
 * in-signals and out-signals. Multiple out-signals for each node
 * are merged into one in-signal per node which are later sent to nodes. This merging
 * takes place in a list where all nodes yet to traverse are stored.
 * There is also list of already traversed nodes. Signals to already traversed
 * nodes are ignored.
  * ND - node data
  * ED - edge data
  * S - signal
  * R - result
 **/
class Traverser[ND, ED, S, R](graphView: GraphView[ND, ED]) {
  import Traverser._
  type ThisTraverser = Traverser[ND, ED, S, R]
  type ThisGraphView = GraphView[ND, ED]
  type ThisNodeInfo = NodeInfo[ND]
  type ThisEdgeInfo = EdgeInfo[ED]
  // type NSet = Set[NodeIDDesignator]
  // type ESet = Set[EdgeIDDesignator]
  // type ESignal = (EdgeIDDesignator, S)
  // type NSignal = (NodeIDDesignator, S)
  // type NSList = Seq[NSignal]
  // type ESList = Seq[ESignal]
  // type Signals = SeqMap[NodeIDDesignator, S]
  // case class EPropagation(toHead: ESList, toTail: ESList)
  // case class NPropagation(toHead: NSList, toTail: NSList)
  val emptyNProp = NPropagation[S](Nil, Nil)

  /**
   * Function determining how signal propagates through node.
   */
  // type TravFun = (S, ThisGraphView, ThisTraverser) => EPropagation

  /**
   * Function handling node traversal.
   */
  type NodeHandleFun = (ThisNodeInfo, S, ThisGraphView, ThisTraverser, R) => (EPropagation[S], R)

  type EdgeHandleFun = (EdgeIDDesignator, S, ThisGraphView, ThisTraverser, R) => R

  type StimMergeFun = (S, S) => S

  def traverse(
   initNode: NodeDesignator,
   initStim: S,
   stimMergeFun: StimMergeFun,
  //  travFun: TravFun,
   nodeHandleFun: NodeHandleFun,
   edgeHandleFun: EdgeHandleFun,
   initRes: R): R = {
    val id_des = toIdDes(initNode)
    val walk_list = SeqMap.empty.addToHead(id_des, initStim)
    val visited_nodes = Set[NodeIDDesignator]()
    val visited_edges = Set[EdgeIDDesignator]()
    doWalking(walk_list, visited_nodes, visited_edges, nodeHandleFun,
     edgeHandleFun, stimMergeFun,  initRes)
  }

  /**
    * Converts any node designator to id node designator.
    */
  private def toIdDes(nd: NodeDesignator): NodeIDDesignator = nd match {
    case nd @ NodeIDDesignator(id) => nd
    case _ => NodeIDDesignator(graphView.node(nd).get.ID)
  }

  @tailrec
  private def doWalking(
   nodes: Signals[S],
   visitedNodes: NSet,
   visitedEdges: ESet,
   //  travFun: TravFun,
   nodeHandleFun: NodeHandleFun,
   edgeHandleFun: EdgeHandleFun,
   stimMergeFun: StimMergeFun,
   handleRes: R): R = if (nodes.isEmpty) {
   // just passing result from earlier evaluation
   handleRes
  }
  else {
     val in_sig = nodes.head
     val (new_vis_nodes, new_vis_edges, n_propagation, new_res) = graphView.node(in_sig._1) match {
       case Some(node_info) => {
         val new_vis_nodes = visitedNodes + node_info.ID.id
         val stimulus = in_sig._2
         val (e_prop, new_res) = nodeHandleFun(node_info, stimulus, graphView, this, handleRes)
         val new_res2: R = handleEdges(edgeHandleFun, e_prop, visitedEdges, new_res)
         val new_vis_edges = visitedEdges ++ (e_prop.toHead map (_._1)).toSet ++
          (e_prop.toTail map (_._1)).toSet
         val raw_n_prop = ePropToNProp(e_prop, node_info)
         // filtering out signals to already traversed nodes
         val n_prop: NPropagation[S] = filtOutVisNodes(raw_n_prop, new_vis_nodes)
         // temporary result (of type R)
        //  val new_res = handleFun(node_info, stimulus, graphView, this, handleRes)
         (new_vis_nodes, new_vis_edges, n_prop, new_res2)
       }
       case None => (visitedNodes, visitedEdges, emptyNProp, handleRes)
     }
     val new_nodes = mergePropagation(nodes.tail, n_propagation, stimMergeFun)
     doWalking(new_nodes, new_vis_nodes, new_vis_edges, nodeHandleFun, edgeHandleFun, stimMergeFun, new_res)
   }

   private def handleEdges(handleFun: EdgeHandleFun, ePropagation: EPropagation[S], visitedEdges: ESet, handleRes: R): R = {
     // function to filter out already handled (visited) edges
     val filt_f = (es: ESignal[S]) => visitedEdges(es._1)
     // function to handle each edge
     val fun = (handleRes: R, e_sig: ESignal[S]) => handleFun(e_sig._1, e_sig._2, graphView, this, handleRes)
     // handling edges in toHead part
     val res1 = ePropagation.toHead.filterNot(filt_f).foldLeft(handleRes)(fun)
     // handling edges in toTail part
     ePropagation.toTail.filterNot(filt_f).foldLeft(res1)(fun)
   }

   private def ePropToNProp(ePropagation: EPropagation[S], node: NodeInfo[ND]): NPropagation[S] = {
     val nd = node.ID.id
     val map_f = (es: ESignal[S]) => {
       (toIdDes(graphView.oppositeNode(nd, es._1)), es._2)
     }
     val to_head = ePropagation.toHead map map_f
     val to_tail = ePropagation.toTail map map_f
     NPropagation[S](to_head, to_tail)
   }

  /**
    * For each out signal in given propagation merges it with existing in-signals
    * in given list (or creates new in-signal from an out-signal if there is
    * no in-signal for given node).
    * @param nodes
    * @param prop
    * @return
    */
  private def mergePropagation(
   nodes: Signals[S],
   prop: NPropagation[S],
   stimMerge: StimMergeFun): Signals[S] = {
    // handling out signals adding to the head of in signals
    val nodes1 = prop.toHead.foldLeft(nodes) {(nodes, out_sig) => {
      nodes.get(out_sig._1) match {
        // merging out signal to in signal
        case Some(stimulus) => nodes.updated(out_sig._1, stimMerge(stimulus, out_sig._2))
        case _ => nodes.addToHead(out_sig._1, out_sig._2)
      }
    }}

    // handling out signals adding to the tail of in signals
    prop.toTail.foldLeft(nodes1) {(nodes, out_sig) => {
      nodes.get(out_sig._1) match {
        // merging out signal to in signal
        case Some(stimulus) => nodes.updated(out_sig._1, stimMerge(stimulus, out_sig._2))
        case _ => nodes.addToTail(out_sig._1, out_sig._2)
      }
    }}
  }

  /**
   * Filter-outs propagation signals to already traversed nodes
   */
  private def filtOutVisNodes(propagation: NPropagation[S], visitedNodes: NSet): NPropagation[S] = {
    val filt_f = (ns: NSignal[S]) => visitedNodes(ns._1)
    val to_head = propagation.toHead.filterNot(filt_f)
    val to_tail = propagation.toTail.filterNot(filt_f)
    NPropagation[S](to_head, to_tail)
  }
}
