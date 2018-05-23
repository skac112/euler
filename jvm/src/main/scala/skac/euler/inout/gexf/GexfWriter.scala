package skac.euler.inout.gexf
import skac.euler._
import skac.euler.General._
import java.io.Writer

class GexfWriter[ND, ED](Serializer: Serializer[ND, ED]) {

  def write(Graph: Graph[ND, ED], Writer: Writer) {
	val gexf_graph = translateToGexf(Graph)
	gexf_graph.write(Writer)
  }

  def save(Graph: Graph[ND, ED], Filename: String) {
    val gexf_graph = translateToGexf(Graph)
    gexf_graph.save(Filename)
  }

  def getXML(Graph: Graph[ND, ED]): String = translateToGexf(Graph).getXML()

  private def translateToGexf(Graph: Graph[ND, ED]): GexfGraph = {
    import Graph._

    val gexf_graph = new GexfGraph()
    val node_map = collection.mutable.Map[NodeDesignatorComp, GexfNode]()

    Graph.nodes foreach {n => {
      val gexf_node = new GexfNode(n.ID.toString, Serializer.serializeNode(n))
      node_map(NodeDesignatorComp(n)) = gexf_node
      gexf_graph + gexf_node
      //gexf_graph + new GexfNode(n.ID.toString, Serializer.serializeNode(n))
    }}

    Graph.edges foreach {e => {
//      println(node_map(NodeDesignatorComp(e.SrcNode)).da)
//      println(node_map(NodeDesignatorComp(e.DstNode)).da)

      gexf_graph +-> (new GexfEdge(e.ID.toString, Serializer.serializeEdge(e)),
        node_map(NodeDesignatorComp(e.SrcNode)).da, node_map(NodeDesignatorComp(e.DstNode)).da) }}

    Serializer.serializeGraph(Graph, gexf_graph)
    gexf_graph
  }
}
