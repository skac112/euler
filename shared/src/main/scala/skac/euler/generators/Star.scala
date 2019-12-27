package skac.euler.generators

import skac.euler.Graph
import skac.euler.General._
import skac.euler._

/**
 * Generates star graph (http://en.wikipedia.org/wiki/Star_%28graph_theory%29).
 * nodeDataGen should give unique data for each invocation beause generating
 * is bases on this assumption.
 */
class Star[G <: Graph[G, ND, ED], ND, ED](LeavesNum: Int)
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
     }).runS(g).value
  }
}
