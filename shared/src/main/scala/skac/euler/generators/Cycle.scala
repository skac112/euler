package skac.euler.generators

import cats.data.State
import skac.euler.Graph
import skac.euler.General._
import skac.euler._

class Cycle[ND, ED](NodesNum: Int)
 (implicit startGraph: Graph[ND, ED],
 nodeDataGen: Graph[ND, ED] => ND,
 edgeDataGen: Graph[ND, ED] => ED) extends GraphGenerator[ND, ED] {
   override def generate() = stateTrans.runS(startGraph).value

   lazy val stateTrans = for {
     _ <- State[Graph[ND, ED], Unit] {case graph => (graph.clear, ())}
     // dodanie wezlow
     _ <- makeTimes(NodesNum, {graph: Graph[ND, ED] => graph + nodeDataGen(graph)})
     // dodanie krawedzi
     res <- makeTimesWithIdx(NodesNum, {(graph: Graph[ND, ED], index: Int) =>
       val dst_index = (index + 1) % graph.nodeCount
       graph +-> (edgeDataGen(graph), graph.node(index.i).get, graph.node(dst_index.i).get)
     })
   } yield res


    //  g = g.clear
    //  1 to LeavesNum - 1 foreach {_ => {
    //    val leaf = nodeDataGen(g)
    //    g = g + leaf
    //    g = g +-> (edgeDataGen(g), cNode.da, leaf.da)
    //  }}
}
