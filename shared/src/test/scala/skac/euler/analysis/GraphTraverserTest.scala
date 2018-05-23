package skac.euler.analysis

import utest._
import skac.euler._
import skac.euler.generators._
import skac.euler.analysis._
import skac.euler.General._
import skac.euler.analysis.Traverser._

object GraphTraverserTest extends TestSuite {
  val starCnt = 10
  implicit var sg: skac.euler.Graph[Int, String] = new skac.euler.impl.fastindex.immutable.Graph[Int, String]()
  implicit def newNodeData(g: skac.euler.Graph[Int, String]): Int = g.nodeCount + 1
  implicit def newEdgeData(g: skac.euler.Graph[Int, String]): String = (g.edgeCount + 1).toString
  val gen = new Star[Int, String](starCnt)
  val g = gen.generate()
  val empty_g = new skac.euler.impl.fastindex.immutable.Graph[Int, String]()

  def tests = Tests {
    'GraphTraverser - {
      val gt = new GraphTraverser[Int, String, Int, String, Any](g)

      val node_add_fun = (ni: gt.ThisNodeInfo, stim: Any, gv: gt.ThisGraphView, t: gt.ThisTraverser, rg: gt.ResultGraph) => {
        val es_list = gv.edges(ni).toSeq map {ei => (ei.ID.eid, null)}
        val e_prop = EPropagation[Any](es_list, Nil)
        (e_prop, Some(rg.nodeCount))
      }

      val edge_add_fun = (ei: gt.ThisEdgeInfo, s: Any, gv: gt.ThisGraphView, t: gt.ThisTraverser, rg: gt.ResultGraph) => {
        val sn_data = gv.node(ei.SrcNode).get.Data
        val dn_data = gv.node(ei.DstNode).get.Data
        val desc = s"$sn_data -> $dn_data"
        Some(desc)
      }

      val stim_merge_fun = (s1: Any, s2: Any) => s1
      val rg = gt.makeGraph(g.node(0.i).get, null, stim_merge_fun, node_add_fun, edge_add_fun, empty_g)
      println(rg.nodeCount)
      assert(rg.nodeCount == starCnt + 1)
      assert(rg.edgeCount == starCnt)
    }
  }
}
