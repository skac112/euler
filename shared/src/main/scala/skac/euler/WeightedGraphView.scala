package skac.euler
import General._

trait WeightedGraphView[ND, ED, M[_]] extends GraphView[ND, ED, M] {
  import General._

  def getNodeWeight(NodeDes: NodeDesignator): Double
  def getEdgeWeight(EdgeDes: EdgeDesignator): Double
}
