package skac

import cats.data.State

package object euler {
    type GraphTrans[G <: Graph[G, _, _]] = (G) => G
    type GraphTransIdx[G <: Graph[G, _, _]] = (G, Int) => G

  /**
    * Transforms graph specified number od times using specified function.
    * @param count
    * @param f
    * @tparam ND
    * @tparam ED
    * @return
    */
    def makeTimes[G <: Graph[G, ND, ED], ND, ED](count: Int, f: GraphTrans[G]) = State[G, Unit] {
      case g => ((1 to count).foldLeft(g){(graph, i) => f(graph)}, ())
    }

    def makeTimesWithIdx[G <: Graph[G, ND, ED], ND, ED](count: Int, f: GraphTransIdx[G]) = State[G, Unit] {
      case g => ((0 until count).foldLeft(g){(graph, i) => f(graph, i)}, ())
    }
}