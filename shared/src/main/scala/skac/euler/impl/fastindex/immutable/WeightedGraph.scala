//package skac.euler.impl.fastindex.immutable
//
//import skac.euler.{WeightedGraph, _}
//import skac.euler.impl.fastindex.generic._
//
//class WeightedGraph[G[ND, ED] <: WeightedGraph[ND, ED], ND, ED](pNodes: Vector[NodeStruct[ND, ED]] = Vector[NodeStruct[ND, ED]]())
// extends AbstractGraph[G, ND, ED] with skac.euler.WeightedGraph[ND, ED] {
//
//  class NodeInfo[ND](override val ID: Any, override val Data: ND, val Weight: Double) extends skac.euler.NodeInfo(ID, Data) {
//    def copy(Weight: Double = this.Weight) = new NodeInfo(ID, Data, Weight)
//  }
//
//  case class EdgeInfo[ED](override val ID: Any, override val Data: ED, override val SrcNode: NodeDesignator,
//   override val DstNode: NodeDesignator, val Weight: Double) extends skac.euler.EdgeInfo(ID, Data, SrcNode, DstNode) {
//    def copy(Weight: Double = this.Weight) = new EdgeInfo(ID, Data, SrcNode, DstNode, Weight)
//  }
//
//  def getNodeWeight(NodeDes: NodeDesignator) = node(NodeDes).get.asInstanceOf[NodeInfo[ND]].Weight
//
//  def setNodeWeight(NodeDes: NodeDesignator, NewWeight: Double) = {
//    val new_nodes = replaceNodeInfo(NodeDes, n => n.asInstanceOf[NodeInfo[ND]].copy(Weight = NewWeight))
//    newInstance(new_nodes)
//  }
//
//  def getEdgeWeight(EdgeDes: EdgeDesignator) = edge(EdgeDes).get.asInstanceOf[EdgeInfo[ED]].Weight
//
//  def setEdgeWeight(EdgeDes: EdgeDesignator, NewWeight: Double) = {
//    val new_nodes = replaceEdgeInfo(EdgeDes, e => e.asInstanceOf[EdgeInfo[ED]].copy(Weight = NewWeight))
//    newInstance(new_nodes)
//  }
//
//  override def newInstance(nodes: Vector[NodeStruct[ND, ED]]): G = new WeightedGraph[G, ND, ED](nodes)
//}