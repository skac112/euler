/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package skac.euler

import cats.Id

trait DescribedGraph[G <: DescribedGraph[G, ND, ED], ND, ED] extends Graph[G, ND, ED] with DescribedGraphView[ND, ED, Id] {
  import General._

  def setNodeName(NodeDes: NodeDesignator, Name: String): G
  def setNodeDesc(NodeDes: NodeDesignator, Desc: String): G
  def setEdgeName(EdgeDes: EdgeDesignator, Name: String): G
  def setEdgeDesc(EdgeDes: EdgeDesignator, Desc: String): G
}
