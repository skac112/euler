package skac.euler.impl.fastindex.generic

import skac.euler._
import skac.euler.General._

/**
 * Podstawowa abstrakcyjna implementacja grafu zoptymalizowana pod kątem szybkiego dostępu
 * do węzłów na podstawie indeksów oraz szybkiego dostępu do krawędzi
 * danego węzła. Informacja o elementach grafu zapisana jest w ArrayBuffer
 * {@link Nodes}. Elementami tej struktury są obiekty klasy NodeStruct. Każdy
 * obiekt klasy NodeStruct przechowuje informacje o pojedynczym węźle oraz
 * wszystkich krawędziach przyległych. Dzięki temu uzyskuje się szybki dostęp
 * do węzłów na podstawie indeksów oraz szybki dostęp do krawędzi danego węzła.
 * Klasa umożliwia łatwe rozszerzenie z możliwością zastąpienia klas NodeInfo i
 * EdgeInfo ich rozszerzeniami w wewnętrznych obiektach reprezentujących
 * strukturę grafu. W tym celu zdefiniowane są metody {@link newNodeInfo()},
 * {@link newEdgeInfo()}, {@link replaceNodeInfo()} oraz {@link replaceEdgeInfo()}. Klasa stanowi
 * podstawę implementacji mutowalnych i niemutowalnych.
 */
abstract class AbstractGraph[ND, ED] extends Graph[ND, ED] {
  import Graph._
  type NodesType <: Seq[NodeStruct[ND, ED]]
  //type ConcreteNodeStruct = NodeStruct[ND, ED]
  protected val Nodes: NodesType

  def nodeCount = Nodes.size
  def edgeCount = Nodes map {_.outEdges.size} sum

  def node(NodeDes: NodeDesignator) = NodeDes match {
    case NodeIdxDesignator(idx) => Some(Nodes(idx).nodeInfo)
    case NodeIDDesignator(id) => Nodes map {_.nodeInfo} find {_.ID == id}

    case NodeDataDesignator(data: AnyRef) => Nodes map {_.nodeInfo} find {_.Data match {
        case node_data: AnyRef => node_data eq data
        case node_data => node_data == data
      }
    }

    case NodeDataDesignator(data) => Nodes map {_.nodeInfo} find {_.Data == data}
    case NodePredDesignator(pred) => Nodes map {_.nodeInfo} find {pred}
  }

  override def nodesOf(Data: ND) = (Data match {
      case data: AnyRef => Nodes map {_.nodeInfo} filter {_.Data match {
          case node_data: AnyRef => node_data eq data
          case other => other == data
      }}
      case other => Nodes map {_.nodeInfo} filter {_.Data == other}
    }).toSet


  override def edgesOf(Data: ED) = (Data match {
      case data: AnyRef => edges filter {_.Data match {
          case edge_data: AnyRef => edge_data eq data
          case other => other == data
      }}
      case other => edges filter {_.Data == other}
    }).toSet

  override def edges: Set[EdgeInfo[ED]] = (Nodes flatMap {_.outEdges} map {_._2}) toSet

  override def nodes: Iterable[NodeInfo[ND]] = Nodes map {_.nodeInfo} toSet

  override def isEdge(NodeDes1: NodeDesignator, NodeDes2: NodeDesignator, Directed: Boolean): Boolean = {
    // jesli Directed, to uwzględnia tylko krawędzie wyjściowe, jesli nie, to także
    // krawędzie wejściowe
    val node_struct = findNodeStruct(NodeDes1).get;
    (node_struct.outEdges.values map {_.DstNode} exists {_ === NodeDes2}) || (!Directed &&
     (node_struct.inEdges.values map {_.SrcNode} exists {_ === NodeDes2}));
  }

  override def edgeBetween(NodeDes1: NodeDesignator, NodeDes2: NodeDesignator, Directed: Boolean) = {
    val node_struct = findNodeStruct(NodeDes1).get
    node_struct.outEdges.values find {_.DstNode === NodeDes2} match {
      case Some(e) => Some(e)
      case None if (!Directed) => node_struct.inEdges map {_._2} find {_.SrcNode === NodeDes2}
      case _ => None
    }
  }

  override def edgesBetween(NodeDes1: NodeDesignator, NodeDes2: NodeDesignator, Directed: Boolean) = {
    val node_struct = findNodeStruct(NodeDes1).get
    (node_struct.outEdges.values filter {_.DstNode === NodeDes2}) ++
     (if (!Directed) node_struct.inEdges.values filter {_.SrcNode === NodeDes2} else Traversable[EdgeInfo[ED]]())
  }

  def edge(EdgeDes: EdgeDesignator) = EdgeDes match {
    case EdgeIdxDesignator(idx) => Some((edges toSeq)(idx))
    case EdgeIDDesignator(id) => edges find {_.ID == id}
    case EdgeDataDesignator(data) => edges find {_.Data == data}
    case EdgeNodesDesignator(node_des1, node_des2) => findNodeStruct(node_des1).get.outEdges.values find {_.DstNode === node_des2}
    case EdgePredDesignator(pred) => edges find {pred(_)}
  }

  protected def findNodeStruct(NodeDes: NodeDesignator): Option[NodeStruct[ND, ED]] = NodeDes match {
    case NodeIdxDesignator(idx) => Some(Nodes(idx))
    case NodeIDDesignator(id) => Nodes find {_.nodeInfo.ID == id}
    case NodeDataDesignator(data: AnyRef) => Nodes find {_.nodeInfo.Data match {
        case node_data: AnyRef => node_data eq data
        case other => other == data
      }
    }
    case NodeDataDesignator(data: Any) => Nodes find {_.nodeInfo.Data == data}
    case NodePredDesignator(pred) => Nodes find {ns => pred(ns.nodeInfo)}
  }

  override def edgesOfNode(NodeDes: NodeDesignator, Direction: Int = NEIGHBOR_SIDE_BOTH): Set[EdgeInfo[ED]] = {
    val ns = findNodeStruct(NodeDes).get
//    (ns.inEdges.values ++ ns.outEdges.values) toSet
    (ns.inEdges.values.toSet ++ ns.outEdges.values.toSet) toSet
  }

  override def inEdgesOfNode(NodeDes: NodeDesignator) = findNodeStruct(NodeDes).get.inEdges.values toSet
  override def outEdgesOfNode(NodeDes: NodeDesignator) = findNodeStruct(NodeDes).get.outEdges.values toSet
  override def inDegree(NodeDes: NodeDesignator) = findNodeStruct(NodeDes).get.inEdges size
  override def outDegree(NodeDes: NodeDesignator) = findNodeStruct(NodeDes).get.outEdges size

  override def degree(NodeDes: NodeDesignator) = {
    val ns = findNodeStruct(NodeDes).get
    ns.inEdges.size + ns.outEdges.size
  }
}
