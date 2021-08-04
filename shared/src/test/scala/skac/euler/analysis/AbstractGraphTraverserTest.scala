package skac.euler.analysis

import cats.Id
import cats.implicits._
import utest._
import skac.euler._
import skac.euler.generators._
import skac.euler.analysis._
import skac.euler.analysis.Traverser._
import skac.euler.impl.fastindex.immutable.AbstractGraph._
import skac.euler.AutoModifiableGraph._

object AbstractGraphTraverserTest extends TestSuite {
  val starCnt = 10
  type G = skac.euler.impl.fastindex.immutable.Graph[Int, String]
  type GP[ND, ED] = skac.euler.impl.fastindex.immutable.Graph[ND, ED]
  implicit var sg = new skac.euler.impl.fastindex.immutable.Graph[Int, String]()
  implicit def newNodeData(g: GP[Int, String]): Int = g.nodeCount + 1
  implicit def newEdgeData(g: GP[Int, String]): String = (g.edgeCount + 1).toString
//  implicit def newNodeData(g: G): Int = g.nodeCount + 1
//  implicit def newEdgeData(g: G): String = (g.edgeCount + 1).toString
  // wygenerowanie gwiazdy
  val gen = new Star(starCnt, sg, newNodeData, newEdgeData)
  val g = gen.generate()
  val empty_g = new skac.euler.impl.fastindex.immutable.Graph[Int, String]()

  override def tests = Tests {
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

      val gt = new GraphTraverser[Int, String, Int, String, Any, GP[Int, String], G, Id] {


        /**
         * Defines how signal is propagated through edges of a particular node (specified by nInfo) and optionally returns
         * data of new node for produced graph. If new data is specified, new node will be added to produced graph.
         *
         * @param nInfo
         * @param stim
         * @param g
         * @return
         */
        override def nodeAddFun(nInfo: ThisNodeInfo, stim: Any, gm: ThisGraphMapping): (EPropagation[Any], Option[Int]) = {
          val es_list = gm.downSide.edges(nInfo).toSeq map {ei => (ei.ID.eid, null)}
          val e_prop = EPropagation[Any](es_list, Nil)
          (e_prop, Some(gm.downSide.nodeCount))
        }

        override def edgeAddFun(eInfo: ThisEdgeInfo, stim: Any, gm: ThisGraphMapping): Option[String] = {
          val sn_data = graphView.node(eInfo.SrcNode).get.Data
          val dn_data = graphView.node(eInfo.DstNode).get.Data
          val desc = s"$sn_data -> $dn_data"
          Some(desc)
        }

        /**
         * Function handling node traversal.
         */
        override def graphView: G = g

        /**
         * Defines how signals merge.
         *
         * @param stim1
         * @param stim2
         * @return
         */
        override def stimMergeFun(stim1: Any, stim2: Any): Any = stim1
      }

      val rg = gt.applyWithGraph(g.node(0.i).get, null, Map.empty, empty_g)
      println(rg.upSide.nodeCount)
      assert(rg.upSide.nodeCount == starCnt + 1)
      assert(rg.upSide.edgeCount == starCnt)
    }
  }
}
