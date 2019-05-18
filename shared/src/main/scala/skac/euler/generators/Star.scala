package skac.euler.generators

import skac.euler.Graph
import skac.euler.General._
import skac.euler._

/**
 * Generates star graph (http://en.wikipedia.org/wiki/Star_%28graph_theory%29).
 * nodeDataGen should give unique data for each invocation beause generating
 * is bases on this assumption.
 */
class Star[ND, ED](LeavesNum: Int)
 (implicit startGraph: Graph[ND, ED],
 nodeDataGen: Graph[ND, ED] => ND,
 edgeDataGen: Graph[ND, ED] => ED) extends GraphGenerator[ND, ED] {

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
     makeTimes(LeavesNum, {graph: Graph[ND, ED] =>
       val leaf = nodeDataGen(graph)
       graph + leaf +-> (edgeDataGen(graph), cNode.da, leaf.da)
     }).runS(g).value

    //  (1 to LeavesNum).foldLeft(g)({(graph, i) => {
    //    val leaf = nodeDataGen(g)
    //    graph + leaf +-> (edgeDataGen(g), cNode.da, leaf.da)
    //  }})

    //  1 to LeavesNum foreach {_ => {
    //    val leaf = nodeDataGen(g)
    //    g = g + leaf
    //    g = g +-> (edgeDataGen(g), cNode.da, leaf.da)
    //  }}
    //  g
  }

  // override def generate = {
  //   var Graph: Graph[ND, ED] = startGraph
  //   Graph = Graph.clear
  //   InternalNode = nodeDataGen()
  //   Graph = Graph + InternalNode
  //   1 to LeavesNum foreach {_ => {
  //     val leaf = nodeDataGen()
  //     Graph = Graph + leaf
  //     Graph = Graph +-> (edgeDataGen(), InternalNode.da, leaf.da)
  //   }}
  //   Graph
  // }
}
