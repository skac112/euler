package skac

import scalaz._

package object euler {
    type GraphTrans[ND, ED] = Graph[ND, ED] => Graph[ND, ED]
    type GraphTransIdx[ND, ED] = (Graph[ND, ED], Int) => Graph[ND, ED]

  /**
    * Transforms graph specified number od times using specified function.
    * @param count
    * @param f
    * @tparam ND
    * @tparam ED
    * @return
    */
    def makeTimes[ND, ED](count: Int, f: GraphTrans[ND, ED]) = State[Graph[ND, ED], Unit] {
      case g => ((1 to count).foldLeft(g){(graph, i) => f(graph)}, ())
    }

    def makeTimesWithIdx[ND, ED](count: Int, f: GraphTransIdx[ND, ED]) = State[Graph[ND, ED], Unit] {
      case g => ((0 until count).foldLeft(g){(graph, i) => f(graph, i)}, ())
    }
}
