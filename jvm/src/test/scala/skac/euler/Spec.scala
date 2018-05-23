package skac.euler

import org.scalatest._
import skac.euler.General._

class Spec extends FlatSpec with Matchers {
}

// class GraphTest extends AssertionsForJUnit {
//
//   @Test
//   def createGraph() {
//     val g = new Graph[Any, Any]
//   }
//
//   @Test
//   def addNodes() {
//     val g = new Graph[Any, Any]
//     for (i <- 1 to 10000)
//       g.addNode(i)
//     assertEquals(g.nodeCount, 10000)
//   }
//
//   @Test
//   def addEdges() {
//     val g = new Graph[Any, Any]
//     for (i <- 1 to 10000)
//       g.addNode(i)
//     assertEquals(g.nodeCount, 10000)
//     for (i <- 1 to 1000)
//       g.addEdge(i, g.NodeIdxDesignator(i), g.NodeIdxDesignator(i + 1))
//     assertEquals(g.edgeCount, 1000)
//   }
// }
