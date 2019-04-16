package skac.euler

trait DescribedGraphView[ND, ED] extends GraphView[ND, ED] {
  import General._

  def getNodeName(NodeDes: NodeDesignator): String
  def getNodeDesc(NodeDes: NodeDesignator): String
  def getEdgeName(EdgeDes: EdgeDesignator): String
  def getEdgeDesc(EdgeDes: EdgeDesignator): String
}
