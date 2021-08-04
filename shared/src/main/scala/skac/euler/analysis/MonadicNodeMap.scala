package skac.euler.analysis

import cats.Monad
import skac.euler.GraphView

object MonadicNodeMap {

}

case class MonadicNodeMap[M[_]: Monad](graphView: GraphView[_, _, M]) {

//  def this(graphView: GraphView[_, _, M]) = this(graphView, Map.empty, Seq.empty)
}
