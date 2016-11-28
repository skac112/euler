package skac.euler.impl.fastindex.mutable

import skac.euler._

class DescWeightedGraph[ND, ED] extends WeightedGraph[ND, ED] with DescribedGraph[ND, ED] {
  import skac.euler.General._

  def getNodeName(NodeDes: NodeDesignator): String = node(NodeDes).get.asInstanceOf[NodeInfo[ND]].Name

  def getNodeDesc(NodeDes: NodeDesignator): String = node(NodeDes).get.asInstanceOf[NodeInfo[ND]].Desc

  def getEdgeName(EdgeDes: EdgeDesignator): String = edge(EdgeDes).get.asInstanceOf[EdgeInfo[ED]].Name

  def getEdgeDesc(EdgeDes: EdgeDesignator): String = edge(EdgeDes).get.asInstanceOf[EdgeInfo[ED]].Desc

  def setNodeName(NodeDes: NodeDesignator, NewName: String) = {
    replaceNodeInfo(NodeDes, (n: skac.euler.General.NodeInfo[ND]) => n.asInstanceOf[NodeInfo[ND]]
                    copy2 (Name = NewName))
    this
  }

  def setNodeDesc(NodeDes: NodeDesignator, NewDesc: String) = {
    replaceNodeInfo(NodeDes, (n: skac.euler.General.NodeInfo[ND]) => n.asInstanceOf[NodeInfo[ND]] copy2 (Desc = NewDesc))
    this
  }

  def setEdgeName(EdgeDes: EdgeDesignator, NewName: String) = {
    replaceEdgeInfo(EdgeDes, (e: skac.euler.General.EdgeInfo[ED]) => e.asInstanceOf[EdgeInfo[ED]] copy2 (Name = NewName))
    this
  }

  def setEdgeDesc(EdgeDes: EdgeDesignator, NewDesc: String) = {
    replaceEdgeInfo(EdgeDes, (e: skac.euler.General.EdgeInfo[ED]) => e.asInstanceOf[EdgeInfo[ED]] copy2 (Desc = NewDesc))
    this
  }

  class NodeInfo[ND](override val ID: Any, override val Data: ND, override val Weight: Double,
   val Name: String, val Desc: String) extends super.NodeInfo(ID, Data, Weight) {

    override def copy(Weight: Double = this.Weight) = new NodeInfo(ID, Data, Weight, Name, Desc)
    def copy2(Name: String = this.Name, Desc: String = this.Desc) = new NodeInfo(ID, Data, Weight, Name, Desc)

  }

  class EdgeInfo[ED](override val ID: Any, override val Data: ED, override val SrcNode: General.NodeDesignator,
   override val DstNode: General.NodeDesignator, override val Weight: Double, val Name: String,
   val Desc: String) extends super.EdgeInfo(ID, Data, SrcNode, DstNode, Weight) {

    override def copy(Weight: Double = this.Weight) = new EdgeInfo(ID, Data, SrcNode, DstNode, Weight, Name, Desc)
    def copy2(Name: String = this.Name, Desc: String = this.Desc) = new EdgeInfo(ID, Data, SrcNode, DstNode, Weight, Name, Desc)

  }

  override def newNodeInfo(Data: ND): NodeInfo[ND] = new NodeInfo(newNodeID, Data, 1.0, "", "")

  override def newEdgeInfo(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator): EdgeInfo[ED] =
    new EdgeInfo(newEdgeID, Data, SrcNode, DstNode, 1.0, "", "")
}
