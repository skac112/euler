/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package skac.euler.impl.fastindex.immutable
import skac.euler._
import skac.euler.impl.fastindex.generic._

case class NodeStruct[+ND, +ED](NodeInfo: NodeInfo[ND], InEdges: Map[Any, EdgeInfo[ED]], OutEdges: Map[Any, EdgeInfo[ED]])
 extends skac.euler.impl.fastindex.generic.NodeStruct[ND, ED] {
   def nodeInfo = NodeInfo
   def inEdges = InEdges
   def outEdges = OutEdges
 }
