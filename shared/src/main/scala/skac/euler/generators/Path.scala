package skac.euler.generators

import cats.data.State
import skac.euler.Graph
import skac.euler.General._
import skac.euler._
//import scalaz._

/**
 * Generates a path graph (https://en.wikipedia.org/wiki/Path_graph).
 */
class Path[G <: Graph[G, ND, ED], ND, ED](nodesNum: Int)
 (implicit startGraph: G,
 nodeDataGen: G => ND,
 edgeDataGen: G => ED) extends GraphGenerator[G, ND, ED] {
   override def generate() = stateTrans.runS(startGraph).value

   lazy val stateTrans = for {
     _ <- State[G, Unit] {case graph => (graph.clear, ())}
     // dodanie wezlow
     _ <- makeTimes[G, ND, ED](nodesNum, {graph: G => graph + nodeDataGen(graph)})
     // dodanie krawedzi
     res <- makeTimesWithIdx[G, ND, ED](nodesNum - 1, {(graph: G, idx: Int) =>
       graph +-> (edgeDataGen(graph), idx.i, (idx + 1).i)})
   } yield res
}
