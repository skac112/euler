package skac.euler

trait WeightedGraphView[ND, ED, M[_]] extends GraphView[ND, ED, M] {
  def getNodeWeight(NodeDes: NodeDesignator): Double
  def getEdgeWeight(EdgeDes: EdgeDesignator): Double
}
