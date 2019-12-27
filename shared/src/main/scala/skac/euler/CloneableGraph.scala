package skac.euler

import General._

trait CloneableGraph[G <: CloneableGraph[G, ND, ED], ND, ED] extends Graph[G, ND, ED] {
  def copy: G
//  def +(Data: ND) = copy += Data
//  def +(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator) = copy += (Data, SrcNode, DstNode)
//  def -(NodeDes: NodeDesignator) = copy -= NodeDes
//  def -(EdgeDes: EdgeDesignator) = copy -= EdgeDes
//  def +(Subgraph: Graph[ND, ED])
}
