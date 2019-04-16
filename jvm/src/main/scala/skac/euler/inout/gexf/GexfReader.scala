package skac.euler.inout.gexf

import skac.euler._
import java.io.Reader
import skac.euler.General._

class GexfReader[ND, ED](Deserializer: Deserializer[ND, ED]) {

//  def open(Filename: String) = {
//    val gexf_graph = new GexfGraph()
//	gexf_graph.open(Filename)
//    translateToGraph(gexf_graph)
//  }
//
//  def read(Reader: Reader) {
//	val gexf_graph = new GexfGraph()
//	gexf_graph.read(Reader)
//    translateToGraph(gexf_graph)
//  }
//
//  private def translateToGraph(GexfGraph: GexfGraph) = {
//    // deserializer tworzy obiekt grafu
//    val graph = Deserializer.deserializeGraph(GexfGraph)
//    val node_map = collection.mutable.Map[NodeDesignator, NodeDesignator]()
//    // deserializer dodaje wezly do grafu
//    GexfGraph.nodes foreach {n => node_map(n.ID.id) = Deserializer.deserializeNode(n.Data)}
//    // deserializer dodaje krawedzie do grafu, otrzymujac info o krawedzi gexf oraz wezle zrodlowym
//    // i docelowym krawedzi
//    GexfGraph.edges foreach {e => Deserializer.deserializeEdge(e.Data, node_map(e.SrcNode), node_map(e.DstNode))}
//    graph
//  }
}
