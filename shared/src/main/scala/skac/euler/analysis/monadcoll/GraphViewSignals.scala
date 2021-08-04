package skac.euler.analysis.monadcoll

import cats.Monad
import cats.implicits._
import skac.euler.{GraphView, NodeDesignator, NodeInfo}

object GraphViewSignals {
  def empty[S, M[_]: Monad](graphView: GraphView[_, _, M]) = new GraphViewSignals[S, M](graphView)
}

/**
  * Map which is also a sequence.
  * @tparam K
  * @tparam V
  */
case class GraphViewSignals[S, M[_]: Monad](graphView: GraphView[_, _, M], map: MonadNodeMap[S, M], seq: Seq[(NodeDesignator, S)]) {
  type ThisGraphViewSignals = GraphViewSignals[S, M]

  def findM(nodeInfo: NodeInfo[_]): M[Option[(NodeDesignator, S)]] = {
    seq.tailRecM[M, Option[(NodeDesignator, S)]] {
      case head :: tail => isNodeDesFor(head._1, nodeInfo).map {
        case true  => Right(Some(head))
        case false => Left(tail)
      }
      case Nil => pure(Right(None))
    }
  }

  def isNodeDesFor(nodeDes: NodeDesignator, nodeInfo: NodeInfo[_]): M[Boolean] = for {
    nd_ni_o <- graphView.node(nodeDes)
  } yield nodeInfo == nd_ni_o.get

  def this(graphView: GraphView[_, _, M]) = this(graphView, MonadNodeMap[S, M](graphView), Seq.empty)

  /**
   * It is assumed that nodeDes doesn't exists in map (by normal key-equality or by comparator value equality).
   * @param nodeDes
   * @param signal
   * @return
   */
  def addToHead(nodeDes: NodeDesignator, signal: S) = new GraphViewSignals[S, M](graphView, map addOnly (nodeDes -> signal), (nodeDes -> signal) +: seq)

  /**
   * It is assumed that nodeDes doesn't exists in map (by normal key-equality or by comparator value equality).
   * @param nodeDes
   * @param signal
   * @return
   */
  def addToTail(nodeDes: NodeDesignator, signal: S) = new GraphViewSignals[S, M](graphView, map addOnly (nodeDes -> signal), seq :+ (nodeDes -> signal))

  def get(nodeDes: NodeDesignator): M[Option[S]] = findPair(nodeDes) map {_.map {_._2}}

  def findPair(nodeDes: NodeDesignator): M[Option[(NodeDesignator, S)]] = map.findPair(nodeDes)

  def updated(nodeDes: NodeDesignator, signal: S): M[GraphViewSignals[S, M]] = for {
    pair_o <- findPair(nodeDes)
    matched_des = pair_o.get._1
    new_map <- map.updated(matched_des, signal)
    seq_idx = seq.indexWhere(_._1 == matched_des)
    new_seq = seq.updated(seq_idx, (matched_des, signal))
  } yield GraphViewSignals[S, M](graphView, new_map, new_seq)

  def isEmpty = seq.isEmpty
  def head: (NodeDesignator, S) = seq.head

  def tail: M[GraphViewSignals[S, M]] = for {
    new_map <- map - seq.head._1
  } yield new GraphViewSignals[S, M](graphView, new_map, seq.tail)

  def pure[T](something: T): M[T] = implicitly[Monad[M]].pure(something)
}
