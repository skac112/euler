package skac.euler

import General._
import cats.{Id, Monad}

import scala.collection.generic._

trait Graph[G <: Graph[G, ND, ED], ND, ED] extends GraphView[ND, ED, Id] {
  override def m = Monad[Id]
  import General._
  import GraphView._

  def nodeCount: Int
  def edgeCount: Int

  /**
   * Returns set of nodes containing specific data.
   */
  def nodesOf(Data: ND): Set[NodeInfo[ND]] = nodes filter {_.Data == Data} toSet

  /**
   * Returnes set of edges containing specific data.
   */
  def edgesOf(Data: ED): Set[EdgeInfo[ED]] = edges filter {_.Data == Data} toSet

  def nodeForIdx(Index: Int): Option[NodeInfo[ND]] = node(NodeIdxDesignator(Index))

  def edgeForIdx(Index: Int): Option[EdgeInfo[ED]] = edge(EdgeIdxDesignator(Index))

  /**
   * Returns all edges of a graph.
   */
  def edges: Iterable[EdgeInfo[ED]] = (0 until edgeCount).view map {edgeForIdx(_).get}

//  override def edges[SED >: ED](nd: NodeDesignator): Set[EdgeInfo[SED]] = (edges filter { isEdgeOf(_, nd) }).toSet[EdgeInfo[SED]]

  override def edges[SED >: ED](nd: NodeDesignator, direction: Int =
   NEIGHBOR_SIDE_BOTH): Set[EdgeInfo[SED]] = (edges filter { isEdgeOf(_, nd, direction)}).toSet[EdgeInfo[SED]]

  override def inEdges[SED >: ED](nd: NodeDesignator): Set[EdgeInfo[SED]] = (edges filter (isInEdgeOfNode(_, nd))).toSet[EdgeInfo[SED]]

  override def outEdges[SED >: ED](nd: NodeDesignator): Set[EdgeInfo[SED]] = (edges filter (isOutEdgeOfNode(_, nd))).toSet[EdgeInfo[SED]]

  /**
   * Returns all nodes of a graph.
   */
  def nodes: Iterable[NodeInfo[ND]] = (0 until nodeCount).view map {nodeForIdx(_).get}
  def addNode(Data: ND): G
  def addEdge(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator): G
  def removeNode(NodeDes: NodeDesignator): G
  def removeNode(NodeData: ND): G = removeNode(NodeDataDesignator(NodeData))
  def removeEdge(EdgeDes: EdgeDesignator): G
  def removeEdge(EdgeData: ED): G = removeEdge(EdgeDataDesignator(EdgeData))

  def reverseEdge(EdgeDes: EdgeDesignator): G = {
    val e = edge(EdgeDes).get
    val g = removeEdge(EdgeDes)
    g.addEdge(e.Data, e.DstNode, e.SrcNode)
  }

  /**
   * Usuwa wszystkie elementy z grafu. W zależności od implementacji metody {@link removeNode()} może
   * działać w sposób mutowalny lub niemutowalny. Ze względu na niską wydajność tej implementacji,
   * wskazane jest, aby klasa implementująca nadpisywała tę metodę usuwając dane elementów w sposób
   * specyficzny dla ich wewnętrznej reprezentacji w danej klasie
   */
  def clear: G = (1 to nodeCount).foldLeft(this.asInstanceOf[G]) {(g, idx) => g.removeNode(0.i)}

  def +(Data: ND) = addNode(Data)

  def +->(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator): G = addEdge(Data, SrcNode, DstNode)

  //def +=(Nodes: Traversable[ND]): Graph[ND, ED] = Nodes.foldLeft(this) {(graph, node_data) => graph += node_data}

  def ++(Nodes: ND*): G = Nodes.foldLeft[G](this.asInstanceOf[G]) {(graph, node_data) => graph + node_data}

//  def +=(Edges: Traversable[Tuple3[ED, NodeDesignator, NodeDesignator]]): Graph[ND, ED] = Edges.foldLeft(this) {
//    (graph, edge) => graph += (edge._1, edge._2, edge._3)}

  def ++->(Edges: (ED, NodeDesignator, NodeDesignator)*): G =
    Edges.foldLeft[G](this.asInstanceOf[G]) { (graph, edge) => graph +-> (edge._1, edge._2, edge._3)}

  def -(NodeDes: NodeDesignator): G = removeNode(NodeDes)
  def -->(EdgeDes: EdgeDesignator): G = removeEdge(EdgeDes)

  /**
   * Dodaje do grafu inny graf. Dodanie odbywa sie na zasadzie sumowania zbiorów wezlow i krawedzi, przy
   * czym obowiazuje zasada, ze o tozsamosci elementu decyduja jego dane. Oznacza to, ze jesli w dodawanym
   * grafie wystepuja wezly z danymi, ktore wystepuja takze w wezlach tego grafu lub wystepuja krawedzie
   * z danymi, ktore wystepuja takze w krawedziach tego grafu, to odpowiednie elementy nie zostana dodane do tego
   * grafu. Nie wystapia wiec powtorzenia danych w ramach wezlow lub krawedzi (o ile nie bylo ich wczesniej w tym grafie).
   * Umozliwia to sensowne dodawanie grafow zgodnie z teoriomnogosciowymi intuicjami.
   */
  def ++(other: Graph[_, ND, ED]): G = {
    // adding nodes - each node from other grah is added only when data of
    // this node does not exists in intermediate graph. For this reason,
    // possible data duplication from other graph are removed (but not these from
    // this graph)
    val new_graph_2 = other.nodes.foldLeft[G](this.asInstanceOf[G]) {(graph, node) => graph.node(node.Data.da) match {
      case Some(ex_node) => graph
      case _ => graph + node.Data }}

    // adding edges - each edge of other graph is added only when data of this
    // edge does not exists in intermediate graph. For this reason,
    // possible data duplication from other graph are removed (but not these
    // from these graph)
    other.edges.foldLeft(new_graph_2) {(graph, edge) => graph.edge(edge.Data.eda) match {
      case Some(ex_node) => graph
      case _ => graph +-> (edge.Data, edge.SrcNode, edge.DstNode)}
    }
  }

  /**
   * Adds another graph to this graph not caring about data duplicates, so
   * effectively it creates a component in this graph containing exactly the
   * other graph.
   */
  def +++(other: Graph[_, ND, ED]): G = {
    val nodes_map = Map[NodeIDDesignator, NodeIDDesignator]()
    // adding nodes and building node-node map in modified this and
    // other graph
    val (new_graph, nodes_map_2) = other.nodes.foldLeft[(G, Map[NodeIDDesignator, NodeIDDesignator])]((this.asInstanceOf[G], nodes_map)) {
      case ((graph, nodes_map), node) => (graph + node.Data, nodes_map + (node.ID.id -> this.node((this.nodeCount - 1).i).get.ID.id))
    }

    other.edges.foldLeft[G](new_graph) {(graph, edge) =>
      val inc_nodes = other.incident(edge)
      val node_des_col = List(inc_nodes._1, inc_nodes._2).map {(ni: NodeInfo[ND]) => nodes_map_2(ni.ID.id)}
      graph +-> (edge.Data, node_des_col(0), node_des_col(1))
    }
  }

  /**
   * Odejmuje graf od grafu reprezentowanego przez tę instancję. W zależności od implementacji metod
   * {@link removeNode()} i {@link removeEdge()} może działać zarówno w sposób mutowalny (w którym
   * odejmowanie dokonywane jest na mutowalnej bieżącej instancji i zwracana jest bieżąca instancja)
   * albo w sposób niemutowalny, w którym odejmowanie następuje na kopii bieżącej instancji.
   */
  def --(other: Graph[_, ND, ED]): G = {
    val new_graph_1 = this.asInstanceOf[G]
    // usunicie węzłów z wynikowego grafu
    val new_graph_2 = other.nodes.foldLeft(new_graph_1) { (graph, node) => graph - node.Data.da}
    // usunicie krawędzi z wynikowego grafu
    other.edges.foldLeft(new_graph_2) { (graph, edge) => graph --> edge.Data.eda}
  }

  override def equals(other: Any) = other match {
    case that: Graph[_, ND, ED] => ((that.nodes map {_.Data}) == (this.nodes map {_.Data})) &&
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

  override def hashCode = {
    val nodes_hash = (nodes zip (1 to nodeCount)) map {kv: (NodeInfo[ND], Int) => {kv._1.Data.hashCode * kv._2}} sum
    val edges_hash = (edges zip (1 to edgeCount)) map {kv: (EdgeInfo[ED], Int) => {kv._1.Data.hashCode * kv._2}} sum;
    nodes_hash + edges_hash
  }

  /**
   * Uaktualnia dane wezla. Wlasciwa implementacja powinna byc zawarta w klasie
   * implementujacej. Powinno byc zachowane id wezla.
   */
  def updateNode(NodeDes: NodeDesignator, NewData: ND): G =
   throw new Exception("Method unimplemented.")

  /**
   * Uaktualnia dane krawedzi. Wlasciwa implementacja powinna byc zawarta w klasie
   * implementujacej. Powinno byc zachowane id krawedzi.
   */
  def updateEdge(EdgeDes: EdgeDesignator, NewData: ED): G =
    throw new Exception("Method unimplemented.")

  /**
   * Zmienia przeciwlegly wzgledem danego wezla wezel krawedzi. Powinny byc
   * zachowane id krawedzi i wezlow
   */
  def chngOppNode(nodeDes: NodeDesignator, edgeDes: EdgeDesignator,
   newOppNodeDes: NodeDesignator): G =
   throw new Exception("Method unimplemented.")

  /**
   * Displays some info about graph.
   */
  def about() {
    println(s"Basic graph characteristics: \r\nNode count: ${nodeCount}, edge count: ${edgeCount}.")
  }

  /**
   * Adds a node and an edge joining source node with newly added node.
   */
  def joinNode(srcNode: NodeDesignator, nodeData: ND, edgeData: ED): G = {
    val g = addNode(nodeData)
    addEdge(edgeData, srcNode, g.nodeCount.i)
  }

  /**
   * Adds another graph as in +++ and joins it with this graph by an edge.
   */
  def joinGraph(srcNode: NodeDesignator, dstNode: NodeDesignator, edgeData: ED,
   otherGraph: Graph[_, ND, ED]): G = ???
}
