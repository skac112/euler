package skac.euler.impl.fastindex.immutable

import skac.euler._
import skac.euler.General._
import skac.euler.impl.fastindex.generic._

class DescWeightedGraph[ND, ED](pNodes: Vector[NodeStruct[ND, ED]] = Vector[NodeStruct[ND, ED]]())
 extends WeightedGraph[ND, ED] with skac.euler.WeightedGraph[ND, ED] with skac.euler.DescribedGraph[ND, ED] {

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

  def getNodeName(NodeDes: NodeDesignator): String = node(NodeDes).get.asInstanceOf[NodeInfo[ND]].Name

  def getNodeDesc(NodeDes: NodeDesignator): String = node(NodeDes).get.asInstanceOf[NodeInfo[ND]].Desc

  def getEdgeName(EdgeDes: EdgeDesignator): String = edge(EdgeDes).get.asInstanceOf[EdgeInfo[ED]].Name

  def getEdgeDesc(EdgeDes: EdgeDesignator): String = edge(EdgeDes).get.asInstanceOf[EdgeInfo[ED]].Desc

  def setNodeName(NodeDes: NodeDesignator, NewName: String) = {
    val new_nodes = replaceNodeInfo(NodeDes, (n => n.asInstanceOf[NodeInfo[ND]] copy2 (Name = NewName)))
    newInstance(new_nodes)
  }

  def setNodeDesc(NodeDes: NodeDesignator, NewDesc: String) = {
    val new_nodes = replaceNodeInfo(NodeDes, (n: skac.euler.General.NodeInfo[ND]) => n.asInstanceOf[NodeInfo[ND]] copy2 (Desc = NewDesc))
    newInstance(new_nodes)
  }

  def setEdgeName(EdgeDes: EdgeDesignator, NewName: String) = {
    val new_nodes = replaceEdgeInfo(EdgeDes, (e: skac.euler.General.EdgeInfo[ED]) => e.asInstanceOf[EdgeInfo[ED]] copy2 (Name = NewName))
    newInstance(new_nodes)
  }

  def setEdgeDesc(EdgeDes: EdgeDesignator, NewDesc: String) = {
    val new_nodes = replaceEdgeInfo(EdgeDes, (e: skac.euler.General.EdgeInfo[ED]) => e.asInstanceOf[EdgeInfo[ED]] copy2 (Desc = NewDesc))
    newInstance(new_nodes)
  }

  override def newInstance(Nodes: Vector[NodeStruct[ND, ED]]) = new DescWeightedGraph[ND, ED](Nodes)
}
