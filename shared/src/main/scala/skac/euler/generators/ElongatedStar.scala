package skac.euler.generators

import skac.euler.Graph
import skac.euler.General._
import skac.euler._
//import scalaz._

/**
 * Generates an "elongated star graph". It is like a star graph now each "ray"
 * can contain more than one node.
 * nodeDataGen should give unique data for each invocation beause generating
 * is bases on this assumption.
 */
class ElongatedStar[G <: Graph[G, ND, ED], ND, ED](LeavesNum: Int, rayNodeCnt: Int)
 (implicit startGraph: G,
 nodeDataGen: G => ND,
 edgeDataGen: G => ED) extends GraphGenerator[G, ND, ED] {
    var g = startGraph

    /**
     * Central node.
     */
    var cNode: ND = _

    override def generate() = {
       g = g.clear
       cNode = nodeDataGen(g)
       g = g + cNode
       // uzycie monady stanu (w makeTimes) - wykonanie funkcji LeavesNum razy
       makeTimes[G, ND, ED](LeavesNum, {graph: G =>
         val leaf = nodeDataGen(graph)
         graph + leaf +-> (edgeDataGen(graph), cNode.da, leaf.da)
       }).runS(g).value}
}
