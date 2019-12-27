package skac.euler.analysis

import cats.Id
import utest._
import skac.euler._
import skac.euler.generators._
import skac.euler.analysis._
import skac.euler.General._
import skac.euler.analysis.Traverser._

object TraverserTest extends TestSuite {
  type G = skac.euler.impl.fastindex.immutable.Graph[Int, String]
  implicit var sg = new skac.euler.impl.fastindex.immutable.Graph[Int, String]()
  implicit def newNodeData(g: skac.euler.Graph[_, Int, String]): Int = g.nodeCount + 1
  implicit def newEdgeData(g: skac.euler.Graph[_, Int, String]): String = (g.edgeCount + 1).toString
  val gen = new Star[G, Int, String](10)
  val g = gen.generate()

  def tests = Tests {
    'HelloWorld - {
      assert(g.nodeCount == 11)
    }

    'CountingNodes - {
      val t = new Traverser[Int, String, Any, Int, Id] {
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

        /**
         * Defines how signal in node propagates to edges and modifies current result.
         *
         * @param nInfo
         * @param stim
         * @param res
         * @return
         */
        override def nodeHandleFun(nInfo: ThisNodeInfo, stim: Any, res: Int): (EPropagation[Any], Int) = {
          val es_list = graphView.edges(nInfo).toSeq map {ei => (ei.ID.eid, null)}
          val e_prop = EPropagation[Any](es_list, Nil)
          (e_prop, res + 1)
        }

        /**
         * Defines how signal in edge modifies current result.
         *
         * @param eidDes
         * @param stim
         * @param res
         * @return
         */
        override def edgeHandleFun(eidDes: EdgeIDDesignator, stim: Any, res: Int): Int = res
      }

//      val node_handle_fun = (ni: NodeInfo[Int], stim: Any, gv: GraphView[Int, String, Id], t: Traverser[Int, String, Any, Int, Id], res: Int) => {
//        val es_list = gv.edges(ni).toSeq map {ei => (ei.ID.eid, null)}
//        val e_prop = EPropagation[Any](es_list, Nil)
//        (e_prop, res + 1)
//      }

//      val edge_handle_fun = (e: EdgeIDDesignator, stim: Any, gv: GraphView[Int, String, Id], t: Traverser[Int, String, Any, Int, Id], res: Int) => res

//      val stim_merge_fun = (s1: Any, s2: Any) => s1
      val res = t(g.node(0.i).get, null, 0)
      assert(res == 11)
    }
  }
}
