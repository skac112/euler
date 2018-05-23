package skac.euler.impl.fastindex.mutable
import skac.euler.General._
import skac.euler.impl.fastindex.generic._

case class NodeStruct[ND, ED](var nodeInfo: NodeInfo[ND],
 var inEdges: Map[Any, EdgeInfo[ED]],
 var outEdges: Map[Any, EdgeInfo[ED]])
 extends skac.euler.impl.fastindex.generic.NodeStruct[ND, ED]
