package skac.euler.impl.fastindex.generic

import skac.euler._

trait NodeStruct[+ND, +ED] {
  def nodeInfo: NodeInfo[ND]
  def inEdges: Map[Any, EdgeInfo[ED]]
  def outEdges: Map[Any, EdgeInfo[ED]]
}
