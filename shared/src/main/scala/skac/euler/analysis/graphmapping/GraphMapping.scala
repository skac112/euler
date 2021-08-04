package skac.euler.analysis.graphmapping

import cats.Monad
import cats.implicits._
import skac.euler.{EdgeDesignator, Graph, GraphView, NodeDesignator, NodeInfo}

trait GraphMapping[GM <: GraphMapping[GM, UND, UED, DND, DED, UG, DG, M], UND, UED, DND, DED, UG <: Graph[UND, UED],
  DG <: GraphView[DND, DED, M],  M[_]] {
  implicit def m: Monad[M]
  def downSide: DG
  def upSide: UG
  def mapDown(upNodeDes: NodeDesignator): NodeDesignator
  def mapDown(upEdgeDes: EdgeDesignator): EdgeDesignator

  def downNode(upNodeDes: NodeDesignator): M[NodeInfo[DND]] = {
    val down_node_des = mapDown(upNodeDes)
    for  {
      node_o <- downSide.node(down_node_des)
    } yield node_o.get
  }
}
