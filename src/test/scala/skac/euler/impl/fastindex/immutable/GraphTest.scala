package skac.euler.impl.fastindex.immutable
import skac.euler.Spec
import skac.euler.Graph._
import skac.euler.General._

class GraphTest extends Spec {
  "An immutable fastindexgraph" should "be created" in {
    val g = new skac.euler.impl.fastindex.immutable.Graph[Any, Any]
  }

  it should "has 1000 nodes after adding 1000 nodes" in {
    val g = new skac.euler.impl.fastindex.immutable.Graph[Any, Any]
    var g1000 = (1 to 1000).foldLeft(g){_ + _.i}
    g1000.nodeCount should be (1000)
  }

  it should "convert properly NodeInfo to NodeDesignator" in {
    val g = new skac.euler.impl.fastindex.immutable.Graph[Any, Any]
    var g1 = g + "one".da
    val ni = g1.node(0.i).get
    val edges = g1.edgesOfNode(ni)
    println(edges)
  }
}
