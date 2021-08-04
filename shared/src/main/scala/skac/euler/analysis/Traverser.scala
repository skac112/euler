package skac.euler.analysis

import cats.Monad
import skac.euler._
import skac.euler.GraphView._
import cats._
import cats.implicits._
import skac.euler.analysis.monadcoll.GraphViewSignals

import scala.annotation._

object Traverser {
  type NSet = Set[NodeIDDesignator]
  type ESet = Set[EdgeDesignator]
  type ESignal[S] = (EdgeDesignator, S)
  type NSignal[S] = (NodeIDDesignator, S)
  type NSList[S] = Seq[NSignal[S]]
  type ESList[S] = Seq[ESignal[S]]
//  type Signals[S, M[_]] = filtOutVisNodes[S, M]
  case class EPropagation[S](toHead: ESList[S], toTail: ESList[S])
  case class NPropagation[S](toHead: NSList[S], toTail: NSList[S])
}

/**
 * Traverses graph view from start node or many start nodes using signals. Signals are propagated from
 * node through incident edges to other (adjacent) nodes. There are two types of signals -
 * in-signals and out-signals. Multiple out-signals for each node
 * are merged into one in-signal per node which are later sent to nodes. This merging
 * takes place in a list where all nodes yet to traverse are stored.
 * There are also lists of already traversed (visited) nodes and edges. Signals to already traversed
 * nodes or edges are ignored. Hence, each node and edge is traversed at most once. Moreover, only nodes and edges
 * from initial component are reachable (can be traversed). Initial component is a graph connected component
 * which initial node belongs to.
  * ND - node data
  * ED - edge data
  * S - signal
  * R - result
 **/
import Traverser._

abstract class Traverser[ND, ED, S, R, GS, GV <: GraphView[ND, ED, M], M[_] : Monad] extends ((NodeDesignator, S, GS, R) => M[R]) {
  type ThisNodeInfo = NodeInfo[ND]
  type ThisEdgeInfo = EdgeInfo[ED]

  def graphView: GV

  /**
    * Defines how signals merge.
    * @param stim1
    * @param stim2
    * @return
    */
  def stimMergeFun(stim1: S, stim2: S): S

  /**
    * Defines how signal in node propagates to incident edges and modifies current result.
    * @param nInfo
    * @param stim
    * @param res
    * @return
    */
  def nodeHandleFun(nInfo: ThisNodeInfo, stim: S, globalSig: GS, res: R): M[(EPropagation[S], GS, R)]

  /**
    * Defines how signal in edge modifies current result.
    * @param edgeDes
    * @param stim
    * @param res
    * @return
    */
  def edgeHandleFun(edgeDes: EdgeDesignator, stim: S, globalSig: GS, res: R): M[(R, GS)]
  val emptyNProp = NPropagation[S](Nil, Nil)

  override def apply(initNode: NodeDesignator,
            initStim: S,
            initGlobalSig: GS,
            initRes: R): M[R] = for {
    id_des <- graphView.idDes(initNode)
    walk_list = GraphViewSignals.empty(graphView).addToHead(id_des.get, initStim)
    visited_nodes = Set[NodeIDDesignator]()
    visited_edges = Set[EdgeDesignator]()
    res <- doWalking(walk_list, visited_nodes, visited_edges, initGlobalSig, initRes)
  } yield res._1

    /**
      * Converts any node designator to id node designator.
      */
//    private def toIdDes(nd: NodeDesignator): NodeIDDesignator = nd match {
//      case nd @ NodeIDDesignator(id) => nd
//      case _ => NodeIDDesignator(graphView.node(nd).get.ID)
//    }

  private def doWalking(nodes: GraphViewSignals[S, M],
                        visitedNodes: NSet,
                        visitedEdges: ESet,
                        globalSig: GS,
                        //  travFun: TravFun,
                        handleRes: R): M[(R, GS)] =
    if (nodes.isEmpty) {
      // just passing result from earlier evaluation
      pure((handleRes, globalSig))
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
              e_prop_new_res <- nodeHandleFun(node_info, stimulus, globalSig, handleRes)
              e_prop = e_prop_new_res._1
              glob_stim_1 = e_prop_new_res._2
              new_res_1 = e_prop_new_res._3
              ehr <- handleEdges(e_prop, visitedEdges, glob_stim_1, new_res_1)
              new_vis_edges = visitedEdges ++ (e_prop.toHead map (_._1)).toSet ++
                (e_prop.toTail map (_._1)).toSet
              raw_n_prop <- ePropToNProp(e_prop, node_info)
              // filtering out signals to already traversed nodes
              n_prop = filtOutVisNodes(raw_n_prop, new_vis_nodes)
            } yield (new_vis_nodes, new_vis_edges, n_prop, ehr._1, ehr._2)
          }
          case None => pure((visitedNodes, visitedEdges, emptyNProp, handleRes, globalSig))
        }
        new_vis_nodes = wave._1
        new_vis_edges = wave._2
        n_propagation = wave._3
        new_res = wave._4
        new_glob_stim = wave._5
        nodes_tail <- nodes.tail
        new_nodes <- mergePropagation(nodes_tail, n_propagation)
        res <- doWalking(new_nodes, new_vis_nodes, new_vis_edges, new_glob_stim, new_res)
      } yield res
    }

  private def handleEdges(ePropagation: EPropagation[S], visitedEdges: ESet, globalSig: GS, handleRes: R): M[(R, GS)] = {
    // function to filter out already handled (visited) edges
    val filt_f = (es: ESignal[S]) => visitedEdges(es._1)
    // function to handle each edge
    val fun = (res_glob_stim: (R, GS), e_sig: ESignal[S]) => edgeHandleFun(e_sig._1, e_sig._2, res_glob_stim._2, res_glob_stim._1)
    for {
      // handling edges in toHead part
      res1 <- ePropagation.toHead.toList.filterNot(filt_f).foldM((handleRes, globalSig))(fun)
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
  }

  /**
    * For each out signal in given propagation merges it with existing in-signals
    * in given list (or creates new in-signal from an out-signal if there is
    * no in-signal for given node).
    * @param nodes
    * @param prop
    * @return
    */
  private def mergePropagation(nodes: GraphViewSignals[S, M],
                               prop: NPropagation[S]): M[GraphViewSignals[S, M]] = for {
    // handling out signals adding to the head of in signals
    nodes1 <- prop.toHead.toList.foldM(nodes) { (nodes, out_sig) => { nodes.get(out_sig._1) flatMap { signal_o => signal_o match {
        // merging out signal to in signal
        case Some(stimulus) => nodes.updated(out_sig._1, stimMergeFun(stimulus, out_sig._2))
        case _ => pure(nodes.addToHead(out_sig._1, out_sig._2))
      }}}}

    // handling out signals adding to the tail of in signals
    nodes2 <- prop.toTail.toList.foldM(nodes1) { (nodes, out_sig) => { nodes.get(out_sig._1) flatMap { signal_o => signal_o match {
        // merging out signal to in signal
        case Some(stimulus) => nodes.updated(out_sig._1, stimMergeFun(stimulus, out_sig._2))
        case _ => pure(nodes.addToTail(out_sig._1, out_sig._2))
      }}}}
  } yield nodes2

  /**
    * Filter-outs propagation signals to already traversed nodes
    */
  private def filtOutVisNodes(propagation: NPropagation[S], visitedNodes: NSet): NPropagation[S] = {
    val filt_f = (ns: NSignal[S]) => visitedNodes(ns._1)
    val to_head = propagation.toHead.filterNot(filt_f)
    val to_tail = propagation.toTail.filterNot(filt_f)
    NPropagation[S](to_head, to_tail)
  }

  def pure[T](something: T): M[T] = implicitly[Monad[M]].pure(something)
}

