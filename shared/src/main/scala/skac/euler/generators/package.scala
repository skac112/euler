package skac.euler

import skac.euler.General._

package object generators {

  private var nextNodeNum: Int = _
  private var nextEdgeNum: Int = _

  def assignWeightsByDegree[G <: WeightedGraph[G, _, _]](startGraph: G, directed: Boolean = false): G = {
    var graph = startGraph
    val factor = 10

    for (i <- 0 until startGraph.nodeCount) graph = {
      val deg = if (directed) graph.inDegree(i.i) else graph.degree(i.i)
      graph.setNodeWeight(i.i, deg * (factor + 1))
    }

    for (i <- 0 until startGraph.edgeCount) {
      val edge = graph.edge(i.ei) get
      val n1 = edge.SrcNode
      graph = graph.setEdgeWeight(i.ei, 0.5 * (graph.degree(edge.SrcNode) + graph.degree(edge.DstNode)))
    }

    graph
  }

  private def sampleDesc = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt."

  def assignDescriptions[G <: DescribedGraph[G, _, _]](startGraph: G): G = {
    var graph = startGraph

    for {i <- 0 until startGraph.nodeCount
         n = startGraph.node(i.i).get} {
         graph = graph.setNodeName(i.i, n.Data.toString).setNodeDesc(i.i, sampleDesc)
    }

    graph
  }

  def intNodeDataGen = {
    nextNodeNum += 1
    nextNodeNum
  }

  def intEdgeDataGen = {
    nextEdgeNum += 1
    nextEdgeNum
  }
}
