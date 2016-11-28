package skac.euler.generators
import skac.euler._
import scala.util.Random
import skac.euler.General._
import skac.euler.Graph._

/**
 * Generuje graf przypadkowy o ustalonej liczbie krawędzi i węzłów. Krawędzie tworzone są pomiędzy
 * losowy wybranymi węzłami.
 */
class RandomGenerator[ND, ED](startGraph: Graph[ND, ED], nodeDataGen: () => ND,
                              edgeDataGen: () => ED, nodeCount: Int, edgeCount: Int, directed: Boolean = false)
 extends GraphGenerator[ND, ED](startGraph, nodeDataGen, edgeDataGen) {

  val random = new Random
  var Graph: Graph[ND, ED] = startGraph

  def generate = {
    1 to nodeCount foreach {_ => Graph = Graph + nodeDataGen()}
    1 to edgeCount foreach {_ => {
        val n1 = pickRandomNode
        var n2 = pickRandomNode

        // sprawdza, czy drugi z wylosowanych węzłów nie jest równy pierwszemu lub czy krawędź
        // łącząca wylosowane węzły (z ew. uwzględnieniem kierunku) już nie występuje
        //while ((n1 eq n2) || Graph.isEdge(NodeIDDesignator(n1.ID), NodeIDDesignator(n2.ID), directed)) {
        while ((n1 eq n2) || Graph.isEdge(n1, n2, directed)) {
          n2 = pickRandomNode
        }

        Graph = Graph+->(edgeDataGen(), NodeIDDesignator(n1.ID), NodeIDDesignator(n2.ID))
    }}
    Graph
  }

  /**
   * Wybiera losowy węzeł.
   */
  def pickRandomNode = Graph.node(random.nextInt(Graph.nodeCount).i) get
}
