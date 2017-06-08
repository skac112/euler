package skac.euler

import General._
import scala.collection.generic._

object Graph {
  implicit def NodeInfo2NodeDesignator(NodeInfo: NodeInfo[_]) = NodeIDDesignator(NodeInfo.ID)
  implicit def EdgeInfo2EdgeDesignator(EdgeInfo: EdgeInfo[_]) = EdgeIDDesignator(EdgeInfo.ID)
  val NEIGHBOR_SIDE_FORWARD = 0;
  val NEIGHBOR_SIDE_BACKWARD = 1;
  val NEIGHBOR_SIDE_BOTH = 2;
}

trait Graph[ND, ED] {
  type G <: Graph[ND, ED]
  import General._
  import Graph._

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

  def nodeCount: Int
  def edgeCount: Int
  def node(NodeDes: NodeDesignator): Option[NodeInfo[ND]]

  /**
   * Zwraca węzły zawierające określone dane.
   */
  def nodesOf(Data: ND): Iterable[NodeInfo[ND]] = nodes filter {_.Data == Data}

  def edge(EdgeDes: EdgeDesignator): Option[EdgeInfo[ED]]

  /**
   * Zwraca zbór krawędzi zawierających określone dane.
   */
  def edgesOf(Data: ED): Iterable[EdgeInfo[ED]] = edges filter {_.Data == Data}

  /**
   * Określa, czy dana krawędź znajduje się pomiędzy danymi węzłami.
   */
  private def edgeIsBetween(Edge: EdgeInfo[ED], NodeDes1: NodeDesignator,
   NodeDes2: NodeDesignator, Directed: Boolean) =
   (Edge.SrcNode === NodeDes1 && Edge.DstNode === NodeDes2 ||
   (!Directed && Edge.SrcNode === NodeDes2 && Edge.DstNode === NodeDes1))

//  private val FunEdgeIsBetween = (Edge: EdgeInfo[ED], NodeDes1: NodeDesignator,
//   NodeDes2: NodeDesignator, Directed: Boolean) =>
//   (Edge.SrcNode == NodeDes1.getID && Edge.DstNode == NodeDes2.getID ||
//   (!Directed && Edge.SrcNode == NodeDes2.getID && Edge.DstNode == NodeDes1.getID))
//
  def nodeForIdx(Index: Int): Option[NodeInfo[ND]] = node(NodeIdxDesignator(Index))

  def edgeForIdx(Index: Int): Option[EdgeInfo[ED]] = edge(EdgeIdxDesignator(Index))

  def edges: Iterable[EdgeInfo[ED]] = (0 until edgeCount) map {edgeForIdx(_).get} toSet

  //def edges: Set[EdgeInfo[ED]] = Traversable.range(0, edgeCount).toSet map {edgeForIdx(_: Int).get}

  def nodes: Iterable[NodeInfo[ND]] = (0 until nodeCount) map {nodeForIdx(_).get} toSet

  //def nodes: Set[NodeInfo[ND]] = Traversable.range(0, edgeCount).toSet map {nodeForIdx(_: Int).get}

  /**
   * Określa, czy między podanymi węzłami znajduje się krawędź. W zależności od
   * parametru Directed szuka krawędzi w kierunku od węzła wskazanego przez
   * NodeDes1 do węzła wskazanego przez NodeDes1 albo krawędzi o dowolnym kierunku
   * pomiędzy tymi węzłami
   */
  def isEdge(NodeDes1: NodeDesignator, NodeDes2: NodeDesignator, Directed: Boolean): Boolean =
    edges exists (edgeIsBetween(_, NodeDes1, NodeDes2, Directed))

  /**
   * Zwraca krawędź pomiędzy danymi węzłami.
   */
  def edgeBetween(NodeDes1: NodeDesignator, NodeDes2: NodeDesignator, Directed: Boolean): Option[EdgeInfo[ED]] =
    edges find (edgeIsBetween(_, NodeDes1, NodeDes2, Directed))

  def edgesBetween(NodeDes1: NodeDesignator, NodeDes2: NodeDesignator, Directed: Boolean): Traversable[EdgeInfo[ED]] =
    edges filter {edgeIsBetween(_, NodeDes1, NodeDes2, Directed)}

  private def isEdgeOfNode(Edge: EdgeInfo[ED], NodeDes: NodeDesignator) =
   Edge.SrcNode === NodeDes || Edge.DstNode === NodeDes

  private def isEdgeOfNode(Edge: EdgeInfo[ED], NodeDes: NodeDesignator, Direction: Int) = Direction match {
    case NEIGHBOR_SIDE_FORWARD => Edge.SrcNode === NodeDes
    case NEIGHBOR_SIDE_BACKWARD => Edge.DstNode === NodeDes
    case NEIGHBOR_SIDE_BOTH => Edge.SrcNode === NodeDes || Edge.DstNode === NodeDes
  }

//  private val FunIsEdgeOfNode = (Edge: EdgeInfo[ED], NodeDes: NodeDesignator) =>
//   Edge.SrcNode == NodeDes.getID || Edge.DstNode == NodeDes.getID

//  def edgesOfNode(NodeDes: NodeDesignator): Traversable[EdgeInfo[ED]] =
//    edges filter (isEdgeOfNode(_, NodeDes))

  def edgesOfNode(NodeDes: NodeDesignator, Direction: Int = NEIGHBOR_SIDE_BOTH) = edges filter {isEdgeOfNode(_, NodeDes, Direction)}

  def degree(NodeDes: NodeDesignator) = edges count (isEdgeOfNode(_, NodeDes))

  private val FunIsInEdgeOfNode = (Edge: EdgeInfo[ED], NodeDes: NodeDesignator) =>
   Edge.DstNode === NodeDes

  def inEdgesOfNode(NodeDes: NodeDesignator) = edges filter (FunIsInEdgeOfNode(_, NodeDes))

  def inDegree(NodeDes: NodeDesignator) = edges count (FunIsInEdgeOfNode(_, NodeDes))

  private val FunIsOutEdgeOfNode = (Edge: EdgeInfo[ED], NodeDes: NodeDesignator) =>
   Edge.SrcNode === NodeDes

  def outEdgesOfNode(NodeDes: NodeDesignator) = edges filter (FunIsOutEdgeOfNode(_, NodeDes))

  def outDegree(NodeDes: NodeDesignator) = edges count (FunIsOutEdgeOfNode(_, NodeDes))

  def neighbors(NodeDes: NodeDesignator, Direction: Int = NEIGHBOR_SIDE_BOTH): Set[NodeInfo[ND]] = {
    val edges = Direction match {
      case NEIGHBOR_SIDE_FORWARD => outEdgesOfNode(NodeDes)
      case NEIGHBOR_SIDE_BACKWARD => inEdgesOfNode(NodeDes)
      case _ => edgesOfNode(NodeDes);
    }
//    edges filterNot (_.isLoop) map (_.oppositeNode(NodeDes)) map (node(_).get) toSet
    edges filterNot {e: EdgeInfo[ED] => edgeIsLoop(e.ID.eid)} map
     {e: EdgeInfo[ED] => oppositeNode(NodeDes, e.ID.eid)} map (node(_).get) toSet
  }

  /**
   * Zwraca zbiór węzłów oddalonych o LayerIdx od węzła określonego przez NodeDes. dla LayerIdx = 0
   * zwraca węzeł NodeDes, dla LayerIdx = 1 zbiór sąsiadów węzła NodeDes itd., dla LayerIdx zbiór
   * sąsiadów sąsiadów (z wyłączeniem sąsiadów i węzła danego) itd.
   */
  def neighborLayer(NodeDes: NodeDesignator, LayerIdx: Int, Direction: Int): Set[NodeInfo[ND]] =
    neighborLayer(Set(NodeDes), LayerIdx, Direction)


  /**
   * Wyznacza n-ta "warstwe" zbioru wezlow, czyli wezly oddalone o n krawedzi od dowolnego wezla
   * poczatkowego zbioru. Dozwolone przejscia po krawedziach (w przod, w tyl lub w obu kierunkach)
   * okreslone sa przez argument Direction
   */
  def neighborLayer(Nodes: Set[NodeDesignator], LayerIdx: Int, Direction: Int): Set[NodeInfo[ND]] = {
    var all_layers = smartNodeColl(Nodes)
    var res = smartNodeColl(Nodes)
    (0 until LayerIdx) foreach ((x: Int) => {
//        println(neighbors(res.NodeDes, Direction).size)
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
  def edgeLayer(Nodes: Set[NodeDesignator], LayerIdx: Int, Direction: Int): Set[EdgeInfo[ED]] = {
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
    ((prev_layer flatMap {ndc: NodeDesignatorComp => edgesOfNode(ndc, Direction)}) toSet) intersect
     ((node_layer flatMap {ndc: NodeDesignatorComp => edgesOfNode(ndc, oppositeDirection(Direction))}) toSet)
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
    case _ => node(NodeDes) match {
        case Some(node_info: NodeInfo[ND]) => Some(node_info.ID)
        case None => None
    }
  }

  def edgeID(EdgeDes: EdgeDesignator): Option[Any] = EdgeDes match {
    case EdgeIDDesignator(id) => Some(id)
    case _ => edge(EdgeDes) match {
        case Some(edge_info: EdgeInfo[ED]) => Some(edge_info.ID)
        case None => None
    }
  }

  def addNode(Data: ND): Graph[ND, ED]
  def addEdge(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator): Graph[ND, ED]
  def removeNode(NodeDes: NodeDesignator): Graph[ND, ED]
  def removeNode(NodeData: ND): Graph[ND, ED] = removeNode(NodeDataDesignator(NodeData))
  def removeEdge(EdgeDes: EdgeDesignator): Graph[ND, ED]
  def removeEdge(EdgeData: ED): Graph[ND, ED] = removeEdge(EdgeDataDesignator(EdgeData))

  def reverseEdge(EdgeDes: EdgeDesignator): Graph[ND, ED] = {
    val e = edge(EdgeDes).get
    val g = removeEdge(EdgeDes)
    g.addEdge(e.Data, e.DstNode, e.SrcNode)
  }

  /**
   * Przekształca kolekcję desygnatorów węzła na kolekcję obiektów {@link NodeDesignatorComp},
   * co umożliwia "sprytne" porównywanie desygnatorów (desygnatory różnych typów mogą odnosić się
   * do tego samego węzła).
   */
  def smartNodeColl[B, That](NodeDesCol: Traversable[NodeDesignator])
   (implicit bf: CanBuildFrom[Traversable[NodeDesignator], B, That]) = NodeDesCol map {n_des => NodeDesignatorComp(n_des)}

  /**
   * Usuwa wszystkie elementy z grafu. W zależności od implementacji metody {@link removeNode()} może
   * działać w sposób mutowalny lub niemutowalny. Ze względu na niską wydajność tej implementacji,
   * wskazane jest, aby klasa implementująca nadpisywała tę metodę usuwając dane elementów w sposób
   * specyficzny dla ich wewnętrznej reprezentacji w danej klasie
   */
  def clear: Graph[ND, ED] = (1 to nodeCount).foldLeft(this) {(g, idx) => g.removeNode(0.i)}

  def +(Data: ND) = addNode(Data)

  def +->(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator) = addEdge(Data, SrcNode, DstNode)

  //def +=(Nodes: Traversable[ND]): Graph[ND, ED] = Nodes.foldLeft(this) {(graph, node_data) => graph += node_data}

  def ++(Nodes: ND*): Graph[ND, ED] = Nodes.foldLeft(this) {(graph, node_data) => graph + node_data}

//  def +=(Edges: Traversable[Tuple3[ED, NodeDesignator, NodeDesignator]]): Graph[ND, ED] = Edges.foldLeft(this) {
//    (graph, edge) => graph += (edge._1, edge._2, edge._3)}

  def ++->(Edges: (ED, NodeDesignator, NodeDesignator)*): Graph[ND, ED] = Edges.foldLeft(this) {
    (graph, edge) => graph +-> (edge._1, edge._2, edge._3)}

  def -(NodeDes: NodeDesignator) = removeNode(NodeDes)
  def -->(EdgeDes: EdgeDesignator) = removeEdge(EdgeDes)

  /**
   * Dodaje do grafu inny graf. Dodanie odbywa sie na zasadzie sumowania zbiorów wezlow i krawedzi, przy
   * czym obowiazuje zasada, ze o tozsamosci elementu decyduja jego dane. Oznacza to, ze jesli w dodawanym
   * grafie wystepuja wezly z danymi, ktore wystepuja takze w wezlach tego grafu lub wystepuja krawedzie
   * z danymi, ktore wystepuja takze w krawedziach tego grafu, to odpowiednie elementy nie zostana dodane do tego
   * grafu. Nie wystapia wiec powtorzenia danych w ramach wezlow lub krawedzi (o ile nie bylo ich wczesniej w tym grafie).
   * Umozliwia to sensowne dodawanie grafow zgodnie z teoriomnogosciowymi intuicjami.
   */
  def ++(Other: Graph[ND, ED]): Graph[ND, ED] = {
    var new_graph = this
    // dzieki temu odjeciu przy dodaniu nie nastapi zdublowanie elementow o tych samych danych
    new_graph = Other -- this
    // dodanie węzłów do wynikowego grafu
    Other.nodes.foldLeft(new_graph) {(graph, node) => graph + node.Data}
    // dodanie krawędzi do wynikowego grafu
    Other.edges.foldLeft(new_graph) {(graph, edge) => graph +-> (edge.Data, edge.SrcNode, edge.DstNode)}
    new_graph
  }

  /**
   * Odejmuje graf od grafu reprezentowanego przez tę instancję. W zależności od implementacji metod
   * {@link removeNode()} i {@link removeEdge()} może działać zarówno w sposób mutowalny (w którym
   * odejmowanie dokonywane jest na mutowalnej bieżącej instancji i zwracana jest bieżąca instancja)
   * albo w sposób niemutowalny, w którym odejmowanie następuje na kopii bieżącej instancji.
   */
  def --(Other: Graph[ND, ED]): Graph[ND, ED] = {
    var new_graph = this
    // usunicie węzłów z wynikowego grafu
    Other.nodes.foldLeft(new_graph) {(graph, node) => graph - node.Data.da}
    // usunicie krawędzi z wynikowego grafu
    Other.edges.foldLeft(new_graph) {(graph, edge) => graph --> edge.Data.eda}
    new_graph
  }

  override def equals(Other: Any) = Other match {
    case that: Graph[ND, ED] => ((that.nodes map {_.Data}) == (this.nodes map {_.Data})) &&
      ((that.edges map {_.Data}) == (this.edges map {_.Data})) && (that.edges forall {e: EdgeInfo[ED] => {
        // funkcja okreslajaca dla kazdej krawedzi warunek rownosci wezla zrodlowego i koncowego
        val that_src_n = that.node(e.SrcNode).get
        val that_dst_n = that.node(e.DstNode).get
        val this_e = this.edge(e.Data.eda).get
        val this_src_n = this.node(this_e.SrcNode).get
        val this_dst_n = this.node(this_e.DstNode).get
        (this_src_n.Data == that_src_n.Data) && (this_dst_n.Data == that_dst_n.Data)
      }})
    case _ => false
  }

  def oppositeNode(NodeDes: NodeDesignator, Edge: EdgeDesignator) = {
    val e_info = edge(Edge).get
    if (e_info.SrcNode === NodeDes) e_info.DstNode else e_info.SrcNode
  }

  def edgeIsLoop(EdgeDes: EdgeDesignator) = {
    val e_info = edge(EdgeDes).get
    e_info.SrcNode === e_info.DstNode
  }

  override def hashCode = {
    val nodes_hash = (nodes zip (1 to nodeCount)) map {kv: (NodeInfo[ND], Int) => {kv._1.Data.hashCode * kv._2}} sum
    val edges_hash = (edges zip (1 to edgeCount)) map {kv: (EdgeInfo[ED], Int) => {kv._1.Data.hashCode * kv._2}} sum;
    nodes_hash + edges_hash
  }

  /**
   * Uaktualnia dane wezla. Wlasciwa implementacja powinna byc zawarta w klasie
   * implementujacej. Powinno byc zachowane id wezla.
   */
  def updateNode(NodeDes: NodeDesignator, NewData: ND): Graph[ND, ED] =
   throw new Exception("Method unimplemented.")

  /**
   * Uaktualnia dane krawedzi. Wlasciwa implementacja powinna byc zawarta w klasie
   * implementujacej. Powinno byc zachowane id krawedzi.
   */
  def updateEdge(EdgeDes: EdgeDesignator, NewData: ED): Graph[ND, ED] =
    throw new Exception("Method unimplemented.")

  /**
   * Zmienia przeciwlegly wzgledem danego wezla wezel krawedzi. Powinny byc
   * zachowane id krawedzi i wezlow
   */
  def chngOppNode(nodeDes: NodeDesignator, edgeDes: EdgeDesignator,
   newOppNodeDes: NodeDesignator): Graph[ND, ED] =
   throw new Exception("Method unimplemented.")

  /**
   * Wyswietla info o grafie
   */
  def about() {
    println(s"Basic graph characteristics: \r\nNode count: ${nodeCount}, edge count: ${edgeCount}.")
  }
}
