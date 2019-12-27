package skac.euler.analysis

import cats.Monad
import skac.euler.General._
import skac.euler._
import skac.euler.GraphView._
import cats._
import cats.implicits._

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
import Traverser._

abstract class Traverser[ND, ED, S, R, M[_] : Monad] extends ((NodeDesignator, S, R) => M[R]) {
  type ThisTraverser = Traverser[ND, ED, S, R, M]
  type ThisGraphView = GraphView[ND, ED, M]
  type ThisNodeInfo = NodeInfo[ND]
  type ThisEdgeInfo = EdgeInfo[ED]
//  implicit def m: Monad[M]

  /**
    * Function handling node traversal.
    */
//  type NodeHandleFun = (ThisNodeInfo, S, ThisGraphView, ThisTraverser, R) => (EPropagation[S], R)

//  type EdgeHandleFun = (EdgeIDDesignator, S, ThisGraphView, ThisTraverser, R) => R

//  type StimMergeFun = (S, S) => S

  def graphView: GraphView[ND, ED, M]

  /**
    * Defines how signals merge.
    * @param stim1
    * @param stim2
    * @return
    */
  def stimMergeFun(stim1: S, stim2: S): S

  /**
    * Defines how signal in node propagates to edges and modifies current result.
    * @param nInfo
    * @param stim
    * @param res
    * @return
    */
  def nodeHandleFun(nInfo: ThisNodeInfo, stim: S, res: R): M[(EPropagation[S], R)]

  /**
    * Defines how signal in edge modifies current result.
    * @param eidDes
    * @param stim
    * @param res
    * @return
    */
  def edgeHandleFun(eidDes: EdgeIDDesignator, stim: S, res: R): M[R]

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

  def apply(initNode: NodeDesignator,
            initStim: S,
            initRes: R): M[R] = for {
    id_des <- graphView.idDes(initNode)
    walk_list = SeqMap.empty.addToHead(id_des.get, initStim)
    visited_nodes = Set[NodeIDDesignator]()
    visited_edges = Set[EdgeIDDesignator]()
    res <- doWalking(walk_list, visited_nodes, visited_edges, initRes)
  } yield res

    /**
      * Converts any node designator to id node designator.
      */
//    private def toIdDes(nd: NodeDesignator): NodeIDDesignator = nd match {
//      case nd @ NodeIDDesignator(id) => nd
//      case _ => NodeIDDesignator(graphView.node(nd).get.ID)
//    }

    private def doWalking(nodes: Signals[S],
                          visitedNodes: NSet,
                          visitedEdges: ESet,
                          //  travFun: TravFun,
                          handleRes: R): M[R] =
    if (nodes.isEmpty) {
      // just passing result from earlier evaluation
      implicitly[Monad[M]].pure(handleRes)
    }
    else {
      val in_sig = nodes.head
      for {
        ni_o <- graphView.node(in_sig._1)
        wave <- ni_o match {
          case Some(node_info) => {
            val new_vis_nodes = visitedNodes + node_info.ID.id
            val stimulus = in_sig._2
            for {
              e_prop_new_res <- nodeHandleFun(node_info, stimulus, handleRes)
              e_prop = e_prop_new_res._1
              new_res = e_prop_new_res._2
              new_res2 <- handleEdges(e_prop, visitedEdges, new_res)
              new_vis_edges = visitedEdges ++ (e_prop.toHead map (_._1)).toSet ++
                (e_prop.toTail map (_._1)).toSet
              raw_n_prop <- ePropToNProp(e_prop, node_info)
              // filtering out signals to already traversed nodes
              n_prop = filtOutVisNodes(raw_n_prop, new_vis_nodes)
            } yield (new_vis_nodes, new_vis_edges, n_prop, new_res2)

          }
          case None => implicitly[Monad[M]].pure((visitedNodes, visitedEdges, emptyNProp, handleRes))
        }
        new_vis_nodes = wave._1
        new_vis_edges = wave._2
        n_propagation = wave._3
        new_res = wave._4
        new_nodes = mergePropagation(nodes.tail, n_propagation)
        res <- doWalking(new_nodes, new_vis_nodes, new_vis_edges, new_res)
      } yield res
    }


//    else {
//      val in_sig = nodes.head
//      val (new_vis_nodes, new_vis_edges, n_propagation, new_res) = graphView.node(in_sig._1) match {
//        case Some(node_info) => {
//          val new_vis_nodes = visitedNodes + node_info.ID.id
//          val stimulus = in_sig._2
//          val (e_prop, new_res) = nodeHandleFun(node_info, stimulus, handleRes)
//          val new_res2: R = handleEdges(e_prop, visitedEdges, new_res)
//          val new_vis_edges = visitedEdges ++ (e_prop.toHead map (_._1)).toSet ++
//            (e_prop.toTail map (_._1)).toSet
//          val raw_n_prop = ePropToNProp(e_prop, node_info)
//          // filtering out signals to already traversed nodes
//          val n_prop: NPropagation[S] = filtOutVisNodes(raw_n_prop, new_vis_nodes)
//          // temporary result (of type R)
//          //  val new_res = handleFun(node_info, stimulus, graphView, this, handleRes)
//          (new_vis_nodes, new_vis_edges, n_prop, new_res2)
//        }
//        case None => (visitedNodes, visitedEdges, emptyNProp, handleRes)
//      }
//      val new_nodes = mergePropagation(nodes.tail, n_propagation)
//      doWalking(new_nodes, new_vis_nodes, new_vis_edges, new_res)
//    }

    private def handleEdges(ePropagation: EPropagation[S], visitedEdges: ESet, handleRes: R): M[R] = {
      // function to filter out already handled (visited) edges
      val filt_f = (es: ESignal[S]) => visitedEdges(es._1)
      // function to handle each edge
      val fun = (handleRes: R, e_sig: ESignal[S]) => edgeHandleFun(e_sig._1, e_sig._2, handleRes)
      for {
        // handling edges in toHead part
        res1 <- ePropagation.toHead.toList.filterNot(filt_f).foldM(handleRes)(fun)
        // handling edges in toTail part
        res <- ePropagation.toTail.toList.filterNot(filt_f).foldM(res1)(fun)
      } yield res
    }

    private def ePropToNProp(ePropagation: EPropagation[S], node: NodeInfo[ND]): M[NPropagation[S]] = {
      val nd = node.ID.id

      val map_f = (es: ESignal[S]) => for {
        opp_node <- graphView.oppositeNode(nd, es._1)
        id_des <- graphView.idDes(opp_node) map { _.get }
      } yield (id_des, es._2)

      for {
        to_head <- ePropagation.toHead.toList traverse map_f
        to_tail <- ePropagation.toTail.toList traverse map_f
      } yield NPropagation(to_head, to_tail)
//
//
//      val to_head = ePropagation.toHead map map_f
//      val to_tail = ePropagation.toTail map map_f
//      NPropagation[S](to_head, to_tail)
    }

  /**
    * For each out signal in given propagation merges it with existing in-signals
    * in given list (or creates new in-signal from an out-signal if there is
    * no in-signal for given node).
    * @param nodes
    * @param prop
    * @return
    */
  private def mergePropagation(nodes: Signals[S],
                               prop: NPropagation[S]): Signals[S] = {
    // handling out signals adding to the head of in signals
    val nodes1 = prop.toHead.foldLeft(nodes) {(nodes, out_sig) => {
      nodes.get(out_sig._1) match {
        // merging out signal to in signal
        case Some(stimulus) => nodes.updated(out_sig._1, stimMergeFun(stimulus, out_sig._2))
        case _ => nodes.addToHead(out_sig._1, out_sig._2)
      }
    }}

    // handling out signals adding to the tail of in signals
    prop.toTail.foldLeft(nodes1) {(nodes, out_sig) => {
      nodes.get(out_sig._1) match {
        // merging out signal to in signal
        case Some(stimulus) => nodes.updated(out_sig._1, stimMergeFun(stimulus, out_sig._2))
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

