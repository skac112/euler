package skac

import cats.data.State

package object euler {
    type GraphTrans[G <: Graph[_, _]] = (G) => G
    type GraphTransIdx[G <: Graph[_, _]] = (G, Int) => G

  /**
    * Transforms graph specified number od times using specified function.
    * @param count
    * @param f
    * @tparam ND
    * @tparam ED
    * @return
    */
    def makeTimes[G <: Graph[ND, ED], ND, ED](count: Int, f: GraphTrans[G]) = State[G, Unit] {
      case g => ((1 to count).foldLeft(g){(graph, i) => f(graph)}, ())
    }

    def makeTimesWithIdx[G <: Graph[ND, ED], ND, ED](count: Int, f: GraphTransIdx[G]) = State[G, Unit] {
      case g => ((0 until count).foldLeft(g){(graph, i) => f(graph, i)}, ())
    }

  sealed trait NodeDesignator
  case class NodeIDDesignator(ID: Any) extends NodeDesignator
  case class NodeIdxDesignator(Index: Int) extends NodeDesignator
  case class NodeDataDesignator[+ND](Data: ND) extends NodeDesignator
  case class NodePredDesignator(Predicate: NodeInfo[_] => Boolean) extends NodeDesignator

  sealed trait EdgeDesignator
  case class EdgeIDDesignator(ID: Any) extends EdgeDesignator
  case class EdgeIdxDesignator(Index: Int) extends EdgeDesignator
  case class EdgeDataDesignator[+ED](Data: ED) extends EdgeDesignator
  case class EdgeNodesDesignator(NodeDes1: NodeDesignator, NodeDes2: NodeDesignator) extends EdgeDesignator
  case class EdgePredDesignator(Predicate: EdgeInfo[_] => Boolean) extends EdgeDesignator

  class EdgeInfo[+ED](val ID: Any, val Data: ED, val SrcNode: NodeDesignator, val DstNode: NodeDesignator) extends EdgeDesignator {
    //    def oppositeNode(NodeDes: NodeDesignator) = if (NodeDes.equals(SrcNode)) DstNode else SrcNode
    //    def isLoop = SrcNode.equals(DstNode)
  }

  class NodeInfo[+ND](val ID: Any, val Data: ND) extends Equals with NodeDesignator {
    def canEqual(other: Any) = {
      other.isInstanceOf[NodeInfo[ND]]
    }

    override def equals(other: Any) = {
      other match {
        case that: NodeInfo[ND] => that.canEqual(NodeInfo.this) && ID == that.ID && Data == that.Data
        case _ => false
      }
    }

    override def hashCode() = {
      val prime = 41
      prime * (prime + ID.hashCode) + Data.hashCode
    }
  }

  /**
   * "Substrat" desygnatora węzła wykorzystujący konwersję implicit do łatwego tworzenia desygnatorów
   * klasy {@link NodeIDDesignator} lub {@link NodeDataDesignator}, np.
   * data.d - zwróci NodeDataDesignator(data)
   * id.id - zwróci NodeIDDesignator(id)
   * Pozwala to uniknąć "przegadanych" konstrukcji typu:
   * Graph.addNode(NodeDataDesignator(data))
   * i zastąpić je np. taką:
   * Graph.addNode(data.d)
   * Takie podejście pozwala z drugiej strony uniknąć dodawania metod z różnymi typami parametrów
   * wskazującymi węzły dla wykonania tych samych operacji
   */
  class NodeDesignatorFactory(Value: Any) {
    def id = NodeIDDesignator(Value)
    def da = NodeDataDesignator(Value)
  }

  class NodeIdxDesignatorFactory(val Value: Int) {
    def i = NodeIdxDesignator(Value)
  }

  class EdgeDesignatorFactory(Value: Any) {
    def eid = EdgeIDDesignator(Value)
    def eda = EdgeDataDesignator(Value)
  }

  class EdgeIdxDesignatorFactory(Value: Int) {
    def ei = EdgeIdxDesignator(Value)
  }

  implicit def Any2NodeDesignatorFactory(Value: Any) = new NodeDesignatorFactory(Value)
  implicit def Int2NodeIdxDesignatorFactory(Value: Int) = new NodeIdxDesignatorFactory(Value)
  implicit def Any2EdgeDesignatorFactory(Value: Any) = new EdgeDesignatorFactory(Value)
  implicit def Int2EdgeIdxDesignatorFactory(Value: Int) = new EdgeIdxDesignatorFactory(Value)
  implicit def Tuple2EdgeNodesDesignator(Value: Tuple2[NodeDesignator, NodeDesignator]) =
    new EdgeNodesDesignator(Value._1, Value._2)

  abstract sealed class SubgraphDesignator

  /**
   * Subgraf określany poprzez zbiór swoich węzłów. Krawędziami subgrafu są wszystkie krawędzie
   * incydentne ze wskazanymi węzłami.
   */
  case class NodesSubgraphDesignator(Nodes: Traversable[NodeDesignator]) extends SubgraphDesignator

  /**
   * Subgraf określany poprzez jawne wskazanie zbioru węzłów i krawędzi. W przypadku, gdy wskazana
   * jest "wisząca" krawędź - tzn. taka, dla której nie wszystkie węzły wskazane są w zbiorze
   * węzłów - do subgrafu dodawane są także brakujące węzły.
   */
  case class ExplicitSubgraphDesignator(Nodes: Traversable[NodeDesignator], Edges: Traversable[NodeDesignator])
    extends SubgraphDesignator

  /**
   * Subgraf określany poprzez predykat wybierający węzły. Krawędziami subgrafu są wszystkie krawędzie
   * incydentne ze wskazanymi węzłami.
   */
  case class PredNodesSubgraphDesignator[ND](Predicate: NodeInfo[ND] => Boolean) extends SubgraphDesignator
}