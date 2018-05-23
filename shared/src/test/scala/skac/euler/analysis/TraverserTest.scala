package skac.euler.analysis

import utest._
import skac.euler._
import skac.euler.generators._
import skac.euler.analysis._
import skac.euler.General._
import skac.euler.analysis.Traverser._

object TraverserTest extends TestSuite {

  implicit var sg: skac.euler.Graph[Int, String] = new skac.euler.impl.fastindex.immutable.Graph[Int, String]()
  implicit def newNodeData(g: skac.euler.Graph[Int, String]): Int = g.nodeCount + 1
  implicit def newEdgeData(g: skac.euler.Graph[Int, String]): String = (g.edgeCount + 1).toString
  val gen = new Star[Int, String](10)
  val g = gen.generate()

  def tests = Tests {
    'HelloWorld - {
      assert(g.nodeCount == 11)
    }

    'CountingNodes - {
      val t = new Traverser[Int, String, Any, Int](g)

      val node_handle_fun = (ni: NodeInfo[Int], stim: Any, gv: GraphView[Int, String], t: Traverser[Int, String, Any, Int], res: Int) => {
        val es_list = gv.edges(ni).toSeq map {ei => (ei.ID.eid, null)}
        val e_prop = EPropagation[Any](es_list, Nil)
        (e_prop, res + 1)
      }

      val edge_handle_fun = (e: EdgeIDDesignator, stim: Any, gv: GraphView[Int, String], t: Traverser[Int, String, Any, Int], res: Int) => res

      val stim_merge_fun = (s1: Any, s2: Any) => s1
      val res = t.traverse(g.node(0.i).get, null, stim_merge_fun, node_handle_fun, edge_handle_fun, 0)
      assert(res == 11)
    }
  }
}
