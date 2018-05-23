package skac.euler.generators

import skac.euler.Graph
import skac.euler.General._
import skac.euler._
import scalaz._

/**
 * Generates a path graph (https://en.wikipedia.org/wiki/Path_graph).
 */
class Path[ND, ED](nodesNum: Int)
 (implicit startGraph: Graph[ND, ED],
 nodeDataGen: Graph[ND, ED] => ND,
 edgeDataGen: Graph[ND, ED] => ED) extends GraphGenerator[ND, ED] {
   var g = startGraph

   override def generate() = stateTrans(g)._1

   lazy val stateTrans = for {
     _ <- State[Graph[ND, ED], Unit] {case g => (g.clear, ())}
     // dodanie wezlow
     _ <- makeTimes(nodesNum, {graph: Graph[ND, ED] => graph + nodeDataGen(graph)})
     // dodanie krawedzi
     res <- makeTimesWithIdx(NodesNum - 1, {(graph: Graph[ND, ED], idx: Int) =>
       graph +-> (edgeDataGen(graph), idx.i, (idx + 1).i))
     })
   } yield res
}
