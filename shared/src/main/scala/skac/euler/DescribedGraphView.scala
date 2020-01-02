package skac.euler

import cats.Monad

trait DescribedGraphView[ND, ED, M[_]] extends GraphView[ND, ED, M] {

  def getNodeName(NodeDes: NodeDesignator): String
  def getNodeDesc(NodeDes: NodeDesignator): String
  def getEdgeName(EdgeDes: EdgeDesignator): String
  def getEdgeDesc(EdgeDes: EdgeDesignator): String
}
