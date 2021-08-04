package skac.euler.analysis.monadcoll

import cats.Monad
import skac.euler.{GraphView, NodeDesignator}

case class MonadNodeSeq[M[_]: Monad](graphView: GraphView[_, _, M], override val baseSeq: Seq[NodeDesignator] = Seq.empty) extends MonadSeq[NodeDesignator, M, MonadNodeSeq[M]] {
  override def newInstance(seq: Seq[NodeDesignator]): MonadNodeSeq[M] = ???

  /**
   * Obtains (packed in a monad) a "comparable" value for given key.
   */
  override def compValueM(k: NodeDesignator): M[Any] = ???

  override def canBeEqual(k1: NodeDesignator, k2: NodeDesignator): Boolean = ???
}
