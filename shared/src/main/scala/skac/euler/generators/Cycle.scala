package skac.euler.generators

import cats.data.State
import skac.euler.Graph
import skac.euler._

class Cycle[G <: ModifiableGraph[G, ND, ED], ND, ED](nodesNum: Int, startGraph: G, nodeDataGen: G => ND,
  edgeDataGen: G => ED) extends GraphGenerator[G, ND, ED](startGraph, nodeDataGen, edgeDataGen) {
   override def generate() = stateTrans.runS(startGraph).value

   lazy val stateTrans = for {
     _ <- State[G, Unit] { case graph => (graph.clear, ()) }
     // dodanie wezlow
     _ <- makeTimes[G, ND, ED](nodesNum, { graph => graph + nodeDataGen(graph) })
     // dodanie krawedzi
     res <- makeTimesWithIdx[G, ND, ED](nodesNum, { (graph: G, index: Int) =>
       val dst_index = (index + 1) % graph.nodeCount
       graph +-> (edgeDataGen(graph), graph.node(index.i).get, graph.node(dst_index.i).get)
     })
   } yield res
}