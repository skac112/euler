/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package skac.euler

trait DescribedGraph[ND, ED] extends Graph[ND, ED] {
  import General._

  def getNodeName(NodeDes: NodeDesignator): String
  def getNodeDesc(NodeDes: NodeDesignator): String
  def getEdgeName(EdgeDes: EdgeDesignator): String
  def getEdgeDesc(EdgeDes: EdgeDesignator): String
  def setNodeName(NodeDes: NodeDesignator, Name: String): DescribedGraph[ND, ED]
  def setNodeDesc(NodeDes: NodeDesignator, Desc: String): DescribedGraph[ND, ED]
  def setEdgeName(EdgeDes: EdgeDesignator, Name: String): DescribedGraph[ND, ED]
  def setEdgeDesc(EdgeDes: EdgeDesignator, Desc: String): DescribedGraph[ND, ED]
}
