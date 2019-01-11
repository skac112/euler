package skac.euler
import General._

trait WeightedGraphView[ND, ED] extends GraphView[ND, ED] {
  import General._

  def getNodeWeight(NodeDes: NodeDesignator): Double
  def getEdgeWeight(EdgeDes: EdgeDesignator): Double
}
