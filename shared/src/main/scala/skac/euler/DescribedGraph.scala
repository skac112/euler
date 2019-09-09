/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package skac.euler

import cats.Id

trait DescribedGraph[ND, ED] extends Graph[ND, ED] with DescribedGraphView[ND, ED, Id] {
  import General._

  def setNodeName(NodeDes: NodeDesignator, Name: String): DescribedGraph[ND, ED]
  def setNodeDesc(NodeDes: NodeDesignator, Desc: String): DescribedGraph[ND, ED]
  def setEdgeName(EdgeDes: EdgeDesignator, Name: String): DescribedGraph[ND, ED]
  def setEdgeDesc(EdgeDes: EdgeDesignator, Desc: String): DescribedGraph[ND, ED]
}
