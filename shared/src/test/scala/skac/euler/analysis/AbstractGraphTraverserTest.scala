package skac.euler.analysis

import cats.Id
import utest._
import skac.euler._
import skac.euler.generators._
import skac.euler.analysis._
import skac.euler.General._
import skac.euler.analysis.Traverser._

object AbstractGraphTraverserTest extends TestSuite {
  val starCnt = 10
  type G = skac.euler.impl.fastindex.immutable.Graph[Int, String]
  implicit var sg = new skac.euler.impl.fastindex.immutable.Graph[Int, String]()
  implicit def newNodeData(g: G): Int = g.nodeCount + 1
  implicit def newEdgeData(g: G): String = (g.edgeCount + 1).toString
  val gen = new Star[G, Int, String](starCnt)
  val g = gen.generate()
  val empty_g = new skac.euler.impl.fastindex.immutable.Graph[Int, String]()

  def tests = Tests {
    'GraphTraverser - {
//      val node_add_fun = (ni: gt.ThisNodeInfo, stim: Any, gv: gt.ThisGraphView, t: gt.ThisTraverser, rg: gt.ThisGraphView) => {
//        val es_list = gv.edges(ni).toSeq map {ei => (ei.ID.eid, null)}
//        val e_prop = EPropagation[Any](es_list, Nil)
//        (e_prop, Some(rg.asInstanceOf[G].nodeCount))
//      }

//      val edge_add_fun = (ei: gt.ThisEdgeInfo, s: Any, gv: gt.ThisGraphView, t: gt.ThisTraverser, rg: gt.ThisGraphView) => {
//        val sn_data = gv.node(ei.SrcNode).get.Data
//        val dn_data = gv.node(ei.DstNode).get.Data
//        val desc = s"$sn_data -> $dn_data"
//        Some(desc)
//      }

//      val stim_merge_fun = (s1: Any, s2: Any) => s1

      val gt = new GraphTraverser[Int, String, Int, String, Any, G, Id] {
        override def nodeAddFun(nInfo: ThisNodeInfo, stim: Any, g: G): (EPropagation[Any], Option[Int]) = {
          val es_list = graphView.edges(nInfo).toSeq map {ei => (ei.ID.eid, null)}
          val e_prop = EPropagation[Any](es_list, Nil)
          (e_prop, Some(g.nodeCount))
        }

        override def edgeAddFun(eInfo: ThisEdgeInfo, stim: Any, g: G): Option[String] = {
          val sn_data = graphView.node(eInfo.SrcNode).get.Data
          val dn_data = graphView.node(eInfo.DstNode).get.Data
          val desc = s"$sn_data -> $dn_data"
          Some(desc)
        }

        /**
         * Function handling node traversal.
         */
        override def graphView: GraphView[Int, String, Id] = g

        /**
         * Defines how signals merge.
         *
         * @param stim1
         * @param stim2
         * @return
         */
        override def stimMergeFun(stim1: Any, stim2: Any): Any = stim1
      }

      val rg = gt(g.node(0.i).get, null, empty_g)
      println(rg.asInstanceOf[G].nodeCount)
      assert(rg.asInstanceOf[G].nodeCount == starCnt + 1)
      assert(rg.asInstanceOf[G].edgeCount == starCnt)
    }
  }
}
