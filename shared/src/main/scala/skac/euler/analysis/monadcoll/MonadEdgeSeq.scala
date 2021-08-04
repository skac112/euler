package skac.euler.analysis.monadcoll

import cats.Monad
import cats.implicits._
import skac.euler.{EdgeDataDesignator, EdgeDesignator, EdgeIDDesignator, EdgeIdxDesignator, GraphView, NodeDesignator}

case class MonadEdgeSeq[M[_]: Monad](
                                      graphView: GraphView[_, _, M],
                                      override val baseSeq: Seq[EdgeDesignator] = Seq.empty)
  extends MonadSeq[EdgeDesignator, M, MonadEdgeSeq[M]] {
  override def newInstance(seq: Seq[EdgeDesignator]): MonadEdgeSeq[M] = MonadEdgeSeq(graphView, seq)

  /**
   * Obtains (packed in a monad) a "comparable" value for given key.
   */
  override def compValueM(edgeDes: EdgeDesignator): M[Any] = graphView.edge(edgeDes) flatMap pure

  override def canBeEqual(des1: EdgeDesignator, des2: EdgeDesignator): Boolean = (des1, des2) match {
    case (EdgeIdxDesignator(idx1), EdgeIdxDesignator(idx2)) => idx1 == idx2
    case (EdgeIDDesignator(id1), EdgeIDDesignator(id2)) => id1 == id2
    case (EdgeDataDesignator(data1), EdgeDataDesignator(data2)) => data1 == data2
    case _ => true
  }
}
