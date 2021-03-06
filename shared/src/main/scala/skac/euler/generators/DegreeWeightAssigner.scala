//package skac.euler.generators
//
//import skac.euler._
//
///**
// * Przypisuje wagi węzłom i krawędziom danego grafu. Wagi węzłów równe są ich stopniom. Wagi
// * krawędzi równe są średniej wag węzłów krawędzi.
// */
//class DegreeWeightAssigner[G <: WeightedGraph[G, _, _]] extends ((G, Boolean) => G) {
//
//  override def apply(StartGraph: G, Directed: Boolean): G = {
//    var graph = StartGraph
//
//    for (i <- 0 to StartGraph.nodeCount - 1) graph = graph.setNodeWeight(i.i, graph.degree(i.i))
//    for (i <- 0 to StartGraph.edgeCount - 1) {
//      val edge = graph.edge(i.ei) get
//      val n1 = edge.SrcNode
//      graph = graph.setEdgeWeight(i.ei, 0.5 * (graph.degree(edge.SrcNode) + graph.degree(edge.DstNode)))
//    }
//
//    graph
//  }
//}
