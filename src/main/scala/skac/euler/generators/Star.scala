package skac.euler.generators

import skac.euler.Graph
import skac.euler.General._

/**
 * Generuje graf typu "gwiazda" (http://en.wikipedia.org/wiki/Star_%28graph_theory%29)
 */
class Star[ND, ED](LeavesNum: Int)
 (implicit startGraph: Graph[ND, ED],
 nodeDataGen: Graph[ND, ED] => ND,
 edgeDataGen: Graph[ND, ED] => ED) extends GraphGenerator[ND, ED] {

   var g = startGraph

  /**
   * Wezel centralny.
   */
  var cNode: ND = _

  override def generate() = {     
     g = g.clear
     cNode = nodeDataGen(g)
     g = g + cNode
     1 to LeavesNum foreach {_ => {
       val leaf = nodeDataGen(g)
       g = g + leaf
       g = g +-> (edgeDataGen(g), cNode.da, leaf.da)
     }}
     g
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
