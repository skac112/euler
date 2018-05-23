package skac.euler

import General._

trait CloneableGraph[ND, ED] extends Graph[ND, ED] {
  def copy: Graph[ND, ED]
//  def +(Data: ND) = copy += Data
//  def +(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator) = copy += (Data, SrcNode, DstNode)
//  def -(NodeDes: NodeDesignator) = copy -= NodeDes
//  def -(EdgeDes: EdgeDesignator) = copy -= EdgeDes
//  def +(Subgraph: Graph[ND, ED])
}
