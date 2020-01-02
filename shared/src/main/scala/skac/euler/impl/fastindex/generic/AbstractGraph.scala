package skac.euler.impl.fastindex.generic

import skac.euler._

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
abstract class AbstractGraph[+ND, +ED] extends Graph[ND, ED] {
  import GraphView._
  type NodesT <: Seq[NodeStruct[ND, ED]]
  //type ConcreteNodeStruct = NodeStruct[ND, ED]
  protected val Nodes: NodesT

  override def nodeCount = Nodes.size
  override def edgeCount = Nodes map {_.outEdges.size} sum

  override def node[SND >: ND](nodeDes: NodeDesignator) = nodeDes match {
    case NodeIdxDesignator(idx) => Some(Nodes(idx).nodeInfo)
    case NodeIDDesignator(id) => Nodes map {_.nodeInfo} find {_.ID == id}

    case NodeDataDesignator(data: AnyRef) => Nodes map {_.nodeInfo} find {_.Data match {
        case node_data: AnyRef => node_data eq data
        case node_data => node_data == data
      }
    }

    case NodeDataDesignator(data) => Nodes map {_.nodeInfo} find {_.Data == data}
    case NodePredDesignator(pred) => Nodes map {_.nodeInfo} find {pred}
    case ni: NodeInfo[SND] => Some(ni)
  }

  override def nodesOf[SND >: ND](data: SND): Set[NodeInfo[SND]] = (data match {
      case data: AnyRef => Nodes map {_.nodeInfo} filter {_.Data match {
          case node_data: AnyRef => node_data eq data
          case other => other == data
      }}
      case other => Nodes map {_.nodeInfo} filter {_.Data == other}
    }).toSet


  override def edgesOf[SED >: ED](data: SED): Set[EdgeInfo[SED]]  = (data match {
      case data: AnyRef => edges filter {_.Data match {
          case edge_data: AnyRef => edge_data eq data
          case other => other == data
      }}
      case other => edges filter {_.Data == other}
    }).toSet

  override def edges: Iterable[EdgeInfo[ED]] = (Nodes flatMap {_.outEdges} map {_._2})

  override def nodes: Iterable[NodeInfo[ND]] = Nodes map {_.nodeInfo}

  override def edgeBetween[SED >: ED](nd1: NodeDesignator, nd2: NodeDesignator, directed: Boolean) = {
    val node_struct = findNodeStruct(nd1).get
    val nid2 = idDes(nd2).get
    node_struct.outEdges.values find { ei: EdgeInfo[ED] => idDes(ei.DstNode) == nid2 } match {
      case Some(e) => Some(e)
      case None if (!directed) => node_struct.inEdges.values find { ei: EdgeInfo[ED] => idDes(ei.SrcNode) == nid2 }
      case _ => None
    }
  }

  override def edgesBetween[SED >: ED](nd1: NodeDesignator, nd2: NodeDesignator, directed: Boolean) = {
    val node_struct = findNodeStruct(nd1).get
    val nid2 = idDes(nd2).get
    val out_edges = node_struct.outEdges.values.toSet[EdgeInfo[SED]] filter { ei: EdgeInfo[_] => idDes(ei.DstNode) == nid2 }

    val in_edges = if (!directed) {
      node_struct.inEdges.values.toSet[EdgeInfo[SED]] filter { ei: EdgeInfo[_] => idDes(ei.SrcNode) == nid2 }
    }
    else {
      Set[EdgeInfo[SED]]()
    }

    out_edges | in_edges
  }

  override def edge[SED >: ED](edgeDes: EdgeDesignator) = edgeDes match {
    case EdgeIdxDesignator(idx) => Some((edges toSeq)(idx))
    case EdgeIDDesignator(id) => edges find {_.ID == id}
    case EdgeDataDesignator(data) => edges find {_.Data == data}
    case EdgeNodesDesignator(node_des1, node_des2) => {
      val nid2 = idDes(node_des2).get
      findNodeStruct(node_des1).get.outEdges.values find { ei: EdgeInfo[_] => ei.DstNode == nid2 }
    }
    case EdgePredDesignator(pred) => edges find {pred(_)}
    case ei: EdgeInfo[ED] => Some(ei)
  }

  protected def findNodeStruct(nodeDes: NodeDesignator): Option[NodeStruct[ND, ED]] = nodeDes match {
    case NodeIdxDesignator(idx) => Some(Nodes(idx))
    case NodeIDDesignator(id) => Nodes find {_.nodeInfo.ID == id}
    case NodeDataDesignator(data) if data.isInstanceOf[AnyRef] => {
      val ref_data = data.asInstanceOf[AnyRef]
      Nodes find {_.nodeInfo.Data match {
        case node_data: AnyRef => node_data eq ref_data
        case other => other == data
      }
    }}
    case NodeDataDesignator(data) => Nodes find {_.nodeInfo.Data == data}
    case NodePredDesignator(pred) => Nodes find {ns => pred(ns.nodeInfo)}
    case ni: NodeInfo[_] => Nodes find {_.nodeInfo == ni}
  }

  override def edges[SED >: ED](nd: NodeDesignator, direction: Int = NEIGHBOR_SIDE_BOTH): Set[EdgeInfo[SED]] = {
    val ns = findNodeStruct(nd).get
    (ns.inEdges.values.toSet ++ ns.outEdges.values.toSet) toSet
  }

  override def inEdges[SED >: ED](nodeDes: NodeDesignator) = findNodeStruct(nodeDes).get.inEdges.values.toSet
  override def outEdges[SED >: ED](nodeDes: NodeDesignator) = findNodeStruct(nodeDes).get.outEdges.values.toSet
  override def inDegree(nodeDes: NodeDesignator) = findNodeStruct(nodeDes).get.inEdges size
  override def outDegree(nodeDes: NodeDesignator) = findNodeStruct(nodeDes).get.outEdges size

  override def degree(nodeDes: NodeDesignator) = {
    val ns = findNodeStruct(nodeDes).get
    ns.inEdges.size + ns.outEdges.size
  }
}
