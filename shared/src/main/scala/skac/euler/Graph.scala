package skac.euler

import General._
import scala.collection.generic._

trait Graph[+ND, +ED] extends GraphView[ND, ED] {
  import General._
  import GraphView._

  def nodeCount: Int
  def edgeCount: Int

  /**
   * Returns set of nodes containing specific data.
   */
  def nodesOf[SND >: ND](Data: SND): Set[NodeInfo[SND]] = nodes filter {_.Data == Data} toSet

  /**
   * Returnes set of edges containing specific data.
   */
  def edgesOf[SED >: ED](Data: SED): Set[EdgeInfo[SED]] = edges filter {_.Data == Data} toSet

  def nodeForIdx(Index: Int): Option[NodeInfo[ND]] = node(NodeIdxDesignator(Index))

  def edgeForIdx(Index: Int): Option[EdgeInfo[ED]] = edge(EdgeIdxDesignator(Index))

  /**
   * Returns all edges of a graph.
   */
  def edges: Iterable[EdgeInfo[ED]] = (0 until edgeCount).view map {edgeForIdx(_).get}

//  override def edges[SED >: ED](nd: NodeDesignator): Set[EdgeInfo[SED]] = (edges filter { isEdgeOf(_, nd) }).toSet[EdgeInfo[SED]]

  override def edges[SED >: ED](nd: NodeDesignator, direction: Int =
   NEIGHBOR_SIDE_BOTH): Set[EdgeInfo[SED]] = (edges filter { isEdgeOf(_, nd, direction)}).toSet[EdgeInfo[SED]]

  override def inEdges[SED >: ED](nd: NodeDesignator): Set[EdgeInfo[SED]] = (edges filter (isInEdgeOfNode[SED](_, nd))).toSet[EdgeInfo[SED]]

  override def outEdges[SED >: ED](nd: NodeDesignator): Set[EdgeInfo[SED]] = (edges filter (isOutEdgeOfNode(_, nd))).toSet[EdgeInfo[SED]]

  /**
   * Returns all nodes of a graph.
   */
  def nodes: Iterable[NodeInfo[ND]] = (0 until nodeCount).view map {nodeForIdx(_).get}

  private def isEdgeOf[SED >: ED](ei: EdgeInfo[SED], nd: NodeDesignator) =
   ei.SrcNode === nd || ei.DstNode === nd

  private def isEdgeOf[SED >: ED](ei: EdgeInfo[SED], nd: NodeDesignator, direction: Int) =
   direction match {
    case NEIGHBOR_SIDE_FORWARD => ei.SrcNode === nd
    case NEIGHBOR_SIDE_BACKWARD => ei.DstNode === nd
    case NEIGHBOR_SIDE_BOTH => ei.SrcNode === nd || ei.DstNode === nd}

  private def isInEdgeOfNode[SED >: ED](edge: EdgeInfo[SED], nd: NodeDesignator) = edge.DstNode === nd
  protected def isOutEdgeOfNode[SED >: ED](edge: EdgeInfo[SED], nd: NodeDesignator) = edge.SrcNode === nd
  def addNode[SND >: ND](Data: SND): Graph[SND, ED]
  def addEdge[SED >: ED](Data: SED, SrcNode: NodeDesignator, DstNode: NodeDesignator): Graph[ND, SED]
  def removeNode(NodeDes: NodeDesignator): Graph[ND, ED]
  def removeNode[SND >: ND](NodeData: SND): Graph[ND, ED] = removeNode(NodeDataDesignator(NodeData))
  def removeEdge(EdgeDes: EdgeDesignator): Graph[ND, ED]
  def removeEdge[SED >: ED](EdgeData: SED): Graph[ND, ED] = removeEdge(EdgeDataDesignator(EdgeData))

  def reverseEdge(EdgeDes: EdgeDesignator): Graph[ND, ED] = {
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
  def clear: Graph[ND, ED] = (1 to nodeCount).foldLeft(this) {(g, idx) => g.removeNode(0.i)}

  def +[SND >: ND](Data: SND) = addNode[SND](Data)

  def +->[SED >: ED](Data: SED, SrcNode: NodeDesignator, DstNode: NodeDesignator) = addEdge[SED](Data, SrcNode, DstNode)

  //def +=(Nodes: Traversable[ND]): Graph[ND, ED] = Nodes.foldLeft(this) {(graph, node_data) => graph += node_data}

  def ++[SND >: ND](Nodes: SND*): Graph[SND, ED] = Nodes.foldLeft[Graph[SND, ED]](this) {(graph, node_data) => graph + node_data}

//  def +=(Edges: Traversable[Tuple3[ED, NodeDesignator, NodeDesignator]]): Graph[ND, ED] = Edges.foldLeft(this) {
//    (graph, edge) => graph += (edge._1, edge._2, edge._3)}

  def ++->[SED >: ED](Edges: (SED, NodeDesignator, NodeDesignator)*): Graph[ND, SED] =
    Edges.foldLeft[Graph[ND, SED]](this) { (graph, edge) => graph +-> (edge._1, edge._2, edge._3)}

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
  def ++[SND >: ND, SED >: ED](other: Graph[SND, SED]): Graph[SND, SED] = {
    // adding nodes - each node from other grah is added only when data of
    // this node does not exists in intermediate graph. For this reason,
    // possible data duplication from other graph are removed (but not these from
    // this graph)
    val new_graph_2 = other.nodes.foldLeft[Graph[SND, SED]](this) {(graph, node) => graph.node(node.Data.da) match {
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
  def +++[SND >: ND, SED >: ED](other: Graph[SND, SED]): Graph[SND, SED] = {
    val nodes_map = Map[NodeIDDesignator, NodeIDDesignator]()
    // adding nodes and building node-node map in modified this and
    // other graph
    val (new_graph, nodes_map_2) = other.nodes.foldLeft[(Graph[SND, SED], Map[NodeIDDesignator, NodeIDDesignator])]((this, nodes_map)) {
      case ((graph, nodes_map), node) => (graph + node.Data, nodes_map + (node.ID.id -> this.node((this.nodeCount - 1).i).get.ID.id))
    }

    other.edges.foldLeft(new_graph) {(graph, edge) =>
      val inc_nodes = other.incident(edge)
      val node_des_col = List(inc_nodes._1, inc_nodes._2).map {(ni: NodeInfo[SND]) => nodes_map_2(ni.ID.id)}
      graph +-> (edge.Data, node_des_col(0), node_des_col(1))
    }
  }

  /**
   * Odejmuje graf od grafu reprezentowanego przez tę instancję. W zależności od implementacji metod
   * {@link removeNode()} i {@link removeEdge()} może działać zarówno w sposób mutowalny (w którym
   * odejmowanie dokonywane jest na mutowalnej bieżącej instancji i zwracana jest bieżąca instancja)
   * albo w sposób niemutowalny, w którym odejmowanie następuje na kopii bieżącej instancji.
   */
  def --[SND >: ND, SED >: ED](Other: Graph[SND, SED]): Graph[ND, ED] = {
    var new_graph_1 = this
    // usunicie węzłów z wynikowego grafu
    val new_graph_2 = Other.nodes.foldLeft(new_graph_1) {(graph, node) => graph - node.Data.da}
    // usunicie krawędzi z wynikowego grafu
    val new_graph_3 = Other.edges.foldLeft(new_graph_2) {(graph, edge) => graph --> edge.Data.eda}
    new_graph_3
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

  override def hashCode = {
    val nodes_hash = (nodes zip (1 to nodeCount)) map {kv: (NodeInfo[ND], Int) => {kv._1.Data.hashCode * kv._2}} sum
    val edges_hash = (edges zip (1 to edgeCount)) map {kv: (EdgeInfo[ED], Int) => {kv._1.Data.hashCode * kv._2}} sum;
    nodes_hash + edges_hash
  }

  /**
   * Uaktualnia dane wezla. Wlasciwa implementacja powinna byc zawarta w klasie
   * implementujacej. Powinno byc zachowane id wezla.
   */
  def updateNode[SND >: ND](NodeDes: NodeDesignator, NewData: SND): Graph[ND, ED] =
   throw new Exception("Method unimplemented.")

  /**
   * Uaktualnia dane krawedzi. Wlasciwa implementacja powinna byc zawarta w klasie
   * implementujacej. Powinno byc zachowane id krawedzi.
   */
  def updateEdge[SED >: ED](EdgeDes: EdgeDesignator, NewData: SED): Graph[ND, ED] =
    throw new Exception("Method unimplemented.")

  /**
   * Zmienia przeciwlegly wzgledem danego wezla wezel krawedzi. Powinny byc
   * zachowane id krawedzi i wezlow
   */
  def chngOppNode(nodeDes: NodeDesignator, edgeDes: EdgeDesignator,
   newOppNodeDes: NodeDesignator): Graph[ND, ED] =
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
  def joinNode[SND >: ND, SED >: ED](srcNode: NodeDesignator, nodeData: SND, edgeData: SED): Graph[SND, SED] = {
    val g = addNode(nodeData)
    addEdge[SED](edgeData, srcNode, g.nodeCount.i)
  }

  /**
   * Adds another graph as in +++ and joins it with this graph by an edge.
   */
  def joinGraph[SND >: ND, SED >: ED](srcNode: NodeDesignator, dstNode: NodeDesignator, edgeData: SED,
   otherGraph: Graph[SND, SED]): Graph[ND, ED] = ???

}
