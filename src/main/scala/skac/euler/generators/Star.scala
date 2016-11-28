package skac.euler.generators

import skac.euler.Graph
import skac.euler.General._

/**
 * Generuje graf typu "gwiazda" (http://en.wikipedia.org/wiki/Star_%28graph_theory%29)
 */
class Star[ND, ED](LeavesNum: Int, startGraph: Graph[ND, ED], nodeDataGen: () => ND, edgeDataGen: () => ED) extends
 GraphGenerator[ND, ED](startGraph, nodeDataGen, edgeDataGen) {

  var InternalNode: ND = _

  override def generate = {
    var Graph: Graph[ND, ED] = startGraph
    Graph = Graph.clear
    InternalNode = nodeDataGen()
    Graph = Graph + InternalNode
    1 to LeavesNum foreach {_ => {
      val leaf = nodeDataGen()
      Graph = Graph + leaf
      Graph = Graph +-> (edgeDataGen(), InternalNode.da, leaf.da)
    }}
    Graph
  }
}
