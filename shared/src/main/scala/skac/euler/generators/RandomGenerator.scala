package skac.euler.generators
import skac.euler._
import scala.util.Random
import skac.euler.General._
// import skac.euler.Graph._

/**
 * Generuje graf przypadkowy o ustalonej liczbie krawędzi i węzłów. Krawędzie tworzone są pomiędzy
 * losowo wybranymi węzłami.
 */
class RandomGenerator[ND, ED](nodeCount: Int, edgeCount: Int, directed: Boolean = false)
 (implicit startGraph: Graph[ND, ED],
 nodeDataGen: Graph[ND, ED] => ND,
 edgeDataGen: Graph[ND, ED] => ED)
 extends GraphGenerator[ND, ED] {

  val random = new Random
  var g: G = startGraph

  override def generate() = {
    g = startGraph
    1 to nodeCount foreach {_ => g = g + nodeDataGen(g)}
    1 to edgeCount foreach {_ => {
        val n1 = pickRandomNode
        var n2 = pickRandomNode

        // sprawdza, czy drugi z wylosowanych węzłów nie jest równy pierwszemu lub czy krawędź
        // łącząca wylosowane węzły (z ew. uwzględnieniem kierunku) już nie występuje
        //while ((n1 eq n2) || Graph.isEdge(NodeIDDesignator(n1.ID), NodeIDDesignator(n2.ID), directed)) {
        while ((n1 eq n2) || !g.edgesBetween(n1, n2, directed).isEmpty) {
          n2 = pickRandomNode
        }

        g = g +-> (edgeDataGen(g), NodeIDDesignator(n1.ID), NodeIDDesignator(n2.ID))
    }}
    g
  }

  /**
   * Wybiera losowy węzeł.
   */
  def pickRandomNode = g.node(random.nextInt(g.nodeCount).i) get
}
