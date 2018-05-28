package skac.euler

import General._
import scala.collection.generic._

object GraphView {
  implicit def NodeInfo2NodeDesignator(NodeInfo: NodeInfo[_]) = NodeIDDesignator(NodeInfo.ID)
  implicit def EdgeInfo2EdgeDesignator(EdgeInfo: EdgeInfo[_]) = EdgeIDDesignator(EdgeInfo.ID)
  val NEIGHBOR_SIDE_FORWARD = 0;
  val NEIGHBOR_SIDE_BACKWARD = 1;
  val NEIGHBOR_SIDE_BOTH = 2;
}

/**
 * Something like a graph but only with "local" information exposed and
 * "read-only". Agnostic about structure as a whole (maybe infinite) - collections of all nodes or edges
  * can't be obtained. Base trait for graph.
 */
trait GraphView[ND, ED] {
  type G <: GraphView[ND, ED]
  type ThisNodeInfo = NodeInfo[ND]
  type ThisEdgeInfo = EdgeInfo[ED]
  type Edges = Set[EdgeInfo[ED]]
  type Nodes = Set[NodeDesignator]
  type NodeInfos = Set[NodeInfo[ND]]
  type EdgeInfos = Set[EdgeInfo[ED]]
  import General._
  import GraphView._

  /**
   * Obiekt umożliwiający porównywanie (z wykorzystaniem konwersji implicit) dwóch desygnatorów
   * węzła. Porownanie takie na poziomie desygnatorów nie zawsze jest możliwe, bo przy
   * różnych typach desygnatorów potrzebne sa dane wezla, a wiec potrzebne jest odniesienie
   * do grafu.
   */
  case class NodeDesignatorComp(NodeDes: NodeDesignator) {
    def ===(Other: NodeDesignatorComp): Boolean = (this.NodeDes, Other.NodeDes) match {
      case (NodeIDDesignator(id1), NodeIDDesignator(id2)) => id1 == id2
      case (NodeIdxDesignator(idx1), NodeIdxDesignator(idx2)) => idx1 == idx2
      case (NodeDataDesignator(data1: Any), NodeDataDesignator(data2: Any)) => data1 == data2
      case (des1, des2) => node(des1) == node(des2)
    }

    override def equals(Other: Any) = Other match {
      case nd: NodeDesignator => ===(NodeDesignatorComp(nd))
      case ndc: NodeDesignatorComp => ===(ndc)
      case _ => false
    }
  }

  case class EdgeDesignatorComp(EdgeDes: EdgeDesignator) {
    def ===(Other: EdgeDesignatorComp): Boolean = (this.EdgeDes, Other.EdgeDes) match {
      case (EdgeIDDesignator(id1), EdgeIDDesignator(id2)) => id1 == id2
      case (EdgeIdxDesignator(idx1), EdgeIdxDesignator(idx2)) => idx1 == idx2
      case (EdgeDataDesignator(data1: Any), EdgeDataDesignator(data2: Any)) => data1 == data2
      case (EdgeNodesDesignator(src_node1, dst_node1), EdgeNodesDesignator(src_node2, dst_node2)) =>
       NodeDesignatorComp(src_node1) === NodeDesignatorComp(src_node2) &&
       NodeDesignatorComp(dst_node1) === NodeDesignatorComp(dst_node2)

      case (des1: EdgeDesignator, des2: EdgeDesignator) => edge(des1) == edge(des2)
    }

    override def equals(Other: Any) = Other match {
      case ed: EdgeDesignator => ===(EdgeDesignatorComp(ed))
      case edc: EdgeDesignatorComp => ===(edc)
      case _ => false
    }
  }

  implicit def NodeDesignator2NodeDesignatorComp(NodeDes: NodeDesignator) = NodeDesignatorComp(NodeDes)
  implicit def NodeDesignatorComp2NodeDesignator(NodeDesignatorComp: NodeDesignatorComp) = NodeDesignatorComp.NodeDes
  implicit def EdgeDesignator2EdgeDesignatorComp(EdgeDes: EdgeDesignator) = EdgeDesignatorComp(EdgeDes)
  implicit def NodeInfo2NodeDesignator(NodeInfo: NodeInfo[ND]) = NodeIDDesignator(NodeInfo.ID)
  implicit def NodeInfo2NodeDesignatorComp(NodeInfo: NodeInfo[ND]) = NodeDesignatorComp(NodeInfo.ID.id)
  implicit def EdgeInfo2EdgeDesignator(EdgeInfo: EdgeInfo[ED]) = EdgeIDDesignator(EdgeInfo.ID)
  implicit def EdgeInfo2EdgeDesignatorComp(EdgeInfo: EdgeInfo[ED]) = EdgeDesignatorComp(EdgeInfo.ID.eid)

  def node(nodeDes: NodeDesignator): Option[NodeInfo[ND]]

  def edge(edgeDes: EdgeDesignator): Option[EdgeInfo[ED]]

  def edges(nodeDes: NodeDesignator): EdgeInfos

  /**
   * Określa, czy dana krawędź znajduje się pomiędzy danymi węzłami.
   */
  private def edgeIsBetween(ei: EdgeInfo[ED], nd1: NodeDesignator,
   nd2: NodeDesignator, directed: Boolean) =
    (ei.SrcNode === nd1 && ei.DstNode === nd2 ||
    (!directed && ei.SrcNode === nd2 && ei.DstNode === nd1))

  /**
   * Określa, czy między podanymi węzłami znajduje się krawędź. W zależności od
   * parametru Directed szuka krawędzi w kierunku od węzła wskazanego przez
   * NodeDes1 do węzła wskazanego przez NodeDes2 albo krawędzi o dowolnym kierunku
   * pomiędzy tymi węzłami
   */
  def edgeExists(nd1: NodeDesignator, nd2: NodeDesignator, directed: Boolean): Boolean = {
    val es = if (directed) outEdges(nd1) else edges(nd1)
    es exists (edgeIsBetween(_, nd1, nd2, directed))
  }

  /**
   * Returns edge between two nodes. If there are multiple edges (multigraph),
   * any of them can be returned.
   */
  def edgeBetween(nd1: NodeDesignator, nd2: NodeDesignator, directed: Boolean): Option[EdgeInfo[ED]] = {
    val es: List[EdgeInfo[ED]] = (if (directed) outEdges(nd1) else edges(nd2)).toList
    es match {
      case Nil => None
      case List(_ @ ei, _*) => Some(ei)
    }
  }

  def edgesBetween(nd1: NodeDesignator, nd2: NodeDesignator, directed: Boolean): EdgeInfos =
    if (directed) {
      // edges from node 1 to node 2
      outEdges(nd1) & inEdges(nd2)
    } else {
      // edges from node 1 to node 2 and from node 2 to node 1
      (outEdges(nd1) & inEdges(nd2)) | (outEdges(nd2) & (inEdges(nd1)))
    }

  /**
    * Set of edges of particular node. Parameter direction enables to select in
    * edges, out edges or both types of edges.
    * @param NodeDes
    * @param Direction
    * @return Set of edges of node.
    */
  def edges(nd: NodeDesignator, direction: Int = NEIGHBOR_SIDE_BOTH): EdgeInfos

  def inEdges(nd: NodeDesignator): EdgeInfos = edges(nd, NEIGHBOR_SIDE_BACKWARD)

  def outEdges(nd: NodeDesignator): EdgeInfos = edges(nd, NEIGHBOR_SIDE_FORWARD)

  def degree(nd: NodeDesignator) = edges(nd) size

  def inDegree(nd: NodeDesignator): Int = inEdges(nd) size

  def outDegree(nd: NodeDesignator): Int = outEdges(nd) size

  def neighbors(nd: NodeDesignator, Direction: Int = NEIGHBOR_SIDE_BOTH): NodeInfos =
    edges(nd, Direction) filterNot { edgeIsLoop _ } map { e => node(oppositeNode(nd, e))} collect {
    case Some(ni) => ni}

  /**
   * Zwraca zbiór węzłów oddalonych o LayerIdx od węzła określonego przez NodeDes. dla LayerIdx = 0
   * zwraca węzeł NodeDes, dla LayerIdx = 1 zbiór sąsiadów węzła NodeDes itd., dla LayerIdx zbiór
   * sąsiadów sąsiadów (z wyłączeniem sąsiadów i węzła danego) itd.
   */
  def neighborLayer(NodeDes: NodeDesignator, LayerIdx: Int, Direction: Int): NodeInfos =
    neighborLayer(Set(NodeDes), LayerIdx, Direction)

  /**
   * Wyznacza n-ta "warstwe" zbioru wezlow, czyli wezly oddalone o n krawedzi od dowolnego wezla
   * poczatkowego zbioru. Dozwolone przejscia po krawedziach (w przod, w tyl lub w obu kierunkach)
   * okreslone sa przez argument Direction
   */
  def neighborLayer(Nodes: Set[NodeDesignator], LayerIdx: Int, Direction: Int): NodeInfos = {
    var all_layers = smartNodeColl(Nodes)
    var res = smartNodeColl(Nodes)
    (0 until LayerIdx) foreach ((x: Int) => {
        res  = (res flatMap {ndc: NodeDesignatorComp => {neighbors(ndc.NodeDes, Direction)} map {ni: NodeInfo[ND] =>
           NodeDesignatorComp(ni.ID.id)}
        }).toSet -- all_layers

    	all_layers = all_layers ++ res
    })

    res map (node(_).get) toSet
  }

  /**
   * Wyznacza n-ta warstwe krawedzi w stosunku do wybranego zbioru wezlow. N-ta warstwa krawedzi
   * to warstwa laczaca wezly z warstwy n-1 z wezlami warstyw n z zachowaniem kierunkow dozwolonych
   * przejsc okreslonych przez argument Direction. Tak wiec numerowanie warstw krawedzi zaczyna sie
   * od 1 i 1-sza warstwa oznacza krawedzie pomiedzy wezlami Nodes a 1-sza warstwa wezlow dla Nodes.
   * Przyjecie numeracji warstw krawedzi od 1 sprawia, ze fragment grafu okreslony przez wezly i
   * krawedzie od 0 do okreslonej warstwy sa podgrafem grafu oryginalnego
   */
  def edgeLayer(Nodes: Set[NodeDesignator], LayerIdx: Int, Direction: Int): EdgeInfos = {
    var all_node_layers = smartNodeColl(Nodes)
    var res = Set[EdgeInfo[ED]]()
    var node_layer = smartNodeColl(Nodes)
    var prev_layer = node_layer

    (1 to LayerIdx) foreach ((x: Int) => {
        node_layer  = (node_layer flatMap {ndc: NodeDesignatorComp => {neighbors(ndc.NodeDes, Direction)} map {ni: NodeInfo[ND] =>
           NodeDesignatorComp(ni.ID.id)}
        }).toSet -- all_node_layers

    	all_node_layers = all_node_layers ++ node_layer

        if (x == LayerIdx - 1)
          prev_layer = node_layer
    })

    // prev_layer oznacza wezly warstwy LayerIdx - 1
    // node_layer oznacza wezly warstwy LayerIdx
    // warstwa krawedzi nr LayerIdx to krawedzie pomiedzy tymi dwiema warstwami wezlow
    // krawedzie te sa wyznaczane jako czesc wspolna zbioru krawedzi odpowiednio skierowanych incydentnych z
    // wezlami warstwy LayerIdx - 1 oraz zbioru krawedzi odpowiednio (przeciwnie) skierowanych incydentnych z
    // wezlami warstwy LayerIdx
    ((prev_layer flatMap {ndc: NodeDesignatorComp => edges(ndc, Direction)}) toSet) intersect
     ((node_layer flatMap {ndc: NodeDesignatorComp => edges(ndc, oppositeDirection(Direction))}) toSet)
    //edge_layer map (edge(_).get) toSet
  }

  def edgeLayer(Node: NodeDesignator, LayerIdx: Int, Direction: Int): Set[EdgeInfo[ED]] =
    edgeLayer(Set(Node), LayerIdx, Direction)

  def oppositeDirection(Direction: Int) = Direction match {
    case NEIGHBOR_SIDE_FORWARD => NEIGHBOR_SIDE_BACKWARD
    case NEIGHBOR_SIDE_BACKWARD => NEIGHBOR_SIDE_FORWARD
    case NEIGHBOR_SIDE_BOTH => NEIGHBOR_SIDE_BOTH
  }

  def nodeID(NodeDes: NodeDesignator): Option[Any] = NodeDes match {
    case NodeIDDesignator(id) => Some(id)
    case _ => node(NodeDes) map {_.ID}
  }

  def edgeID(EdgeDes: EdgeDesignator): Option[Any] = EdgeDes match {
    case EdgeIDDesignator(id) => Some(id)
    case _ => edge(EdgeDes) map {_.ID}
  }

  /**
   * Przekształca kolekcję desygnatorów węzła na kolekcję obiektów {@link NodeDesignatorComp},
   * co umożliwia "sprytne" porównywanie desygnatorów (desygnatory różnych typów mogą odnosić się
   * do tego samego węzła).
   */
  def smartNodeColl[B, That](NodeDesCol: Traversable[NodeDesignator])
   (implicit bf: CanBuildFrom[Traversable[NodeDesignator], B, That]) = NodeDesCol map {n_des => NodeDesignatorComp(n_des)}

  def oppositeNode(NodeDes: NodeDesignator, Edge: EdgeDesignator): NodeDesignator = {
    val e_info = edge(Edge).get
    if (e_info.SrcNode === NodeDes) e_info.DstNode else e_info.SrcNode
  }

  def edgeIsLoop(edgeDes: EdgeDesignator) = {
    val ei = edge(edgeDes).get
    ei.SrcNode === ei.DstNode
  }

  /**
   * Returns pair of nodes incident to an edge.
   */
  def incident(edgeDes: EdgeDesignator): (ThisNodeInfo, ThisNodeInfo) = {
    val e = edge(edgeDes).get
    (node(e.SrcNode).get, node(e.DstNode).get)    
  }

  /**
   * Returns set of edges adjacent to a given edge.
   */
  def adjacent(edgeDes: EdgeDesignator): Set[ThisEdgeInfo] = ???
}
