//package skac.euler.impl.fastindex.mutable
//
//import skac.euler._
////import skac.euler.General._
//
//class WeightedGraph[ND, ED] extends Graph[ND, ED] with skac.euler.WeightedGraph[ND, ED] {
//  import skac.euler.General._
//
//  def getNodeWeight(NodeDes: NodeDesignator) = node(NodeDes).get.asInstanceOf[NodeInfo[ND]].Weight
//
////  def setNodeWeight(NodeDes: NodeDesignator, NewWeight: Double) =
////    replaceNodeInfo(NodeDes, (n: General.NodeInfo[ND]) => n.asInstanceOf[NodeInfo[ND]] copy (Weight = NewWeight))
//
//  def setNodeWeight(NodeDes: NodeDesignator, NewWeight: Double) = {
//    replaceNodeInfo(NodeDes, _.asInstanceOf[NodeInfo[ND]] copy (Weight = NewWeight))
//    this
//  }
//
//  def getEdgeWeight(EdgeDes: EdgeDesignator) = edge(EdgeDes).get.asInstanceOf[EdgeInfo[ED]].Weight
//
//  def setEdgeWeight(EdgeDes: EdgeDesignator, NewWeight: Double) = {
//    replaceEdgeInfo(EdgeDes, (e: General.EdgeInfo[ED]) => e.asInstanceOf[EdgeInfo[ED]] copy (Weight = NewWeight))
//    this
//  }
//
//  //case class NodeInfo[ND](override val ID: Any, override val Data: ND, override val Weight: Double) extends skac.euler.NodeInfo(ID, Data);
//
//  class NodeInfo[ND](override val ID: Any, override val Data: ND, val Weight: Double) extends General.NodeInfo(ID, Data) {
//    def copy(Weight: Double = this.Weight) = new NodeInfo(ID, Data, Weight)
//  }
//
//  case class EdgeInfo[ED](override val ID: Any, override val Data: ED, override val SrcNode: General.NodeDesignator,
//   override val DstNode: General.NodeDesignator, val Weight: Double) extends General.EdgeInfo(ID, Data, SrcNode, DstNode) {
//    def copy(Weight: Double = this.Weight) = new EdgeInfo(ID, Data, SrcNode, DstNode, Weight)
//  }
//
//  override def newNodeInfo(Data: ND): NodeInfo[ND] = new NodeInfo(newNodeID, Data, 1.0)
//
//  override def newEdgeInfo(Data: ED, SrcNode: General.NodeDesignator, DstNode: General.NodeDesignator): EdgeInfo[ED] =
//    new EdgeInfo(newEdgeID, Data, SrcNode, DstNode, 1.0)
//}
