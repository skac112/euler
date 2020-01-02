package skac.euler

trait GraphModifier[G[ND, ED] <: Graph[ND, ED], ND, ED] {
  def addNode(g: G[ND, ED], data: ND): G[ND, ED]
  def addEdge(g: G[ND, ED], data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator): G[ND, ED]
  def removeNode(g: G[ND, ED], nodeDes: NodeDesignator): G[ND, ED]
  def removeEdge(g: G[ND, ED], edgeDes: EdgeDesignator): G[ND, ED]
  def removeNode(g: G[ND, ED], nodeData: ND): G[ND, ED] = removeNode(g, NodeDataDesignator(nodeData))
  def removeEdge(g: G[ND, ED], edgeData: ED): G[ND, ED] = removeEdge(g, EdgeDataDesignator(edgeData))

  def reverseEdge(g: G[ND, ED], edgeDes: EdgeDesignator): G[ND, ED] = {
    val e = g.edge(edgeDes).get
    val g2 = removeEdge(g, edgeDes)
    addEdge(g2, e.Data, e.DstNode, e.SrcNode)
  }

  /**
   * Usuwa wszystkie elementy z grafu. W zależności od implementacji metody {@link removeNode()} może
   * działać w sposób mutowalny lub niemutowalny. Ze względu na niską wydajność tej implementacji,
   * wskazane jest, aby klasa implementująca nadpisywała tę metodę usuwając dane elementów w sposób
   * specyficzny dla ich wewnętrznej reprezentacji w danej klasie
   */
  def clear(g: G[ND, ED]): G[ND, ED] = (1 to g.nodeCount).foldLeft(g) {(graph, idx) => removeNode(graph, 0.i)}

  def +(g: G[ND, ED], data: ND) = addNode(g, data)

  def +->(g: G[ND, ED], data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator): G[ND, ED] = addEdge(g, data, srcNode, dstNode)

  //def +=(Nodes: Traversable[ND]): Graph[ND, ED] = Nodes.foldLeft(this) {(graph, node_data) => graph += node_data}

  def ++(g: G[ND, ED], nodes: ND*): G[ND, ED] = nodes.foldLeft(g) {(graph, node_data) => this + (graph, node_data)}

  //  def +=(Edges: Traversable[Tuple3[ED, NodeDesignator, NodeDesignator]]): Graph[ND, ED] = Edges.foldLeft(this) {
  //    (graph, edge) => graph += (edge._1, edge._2, edge._3)}

  def ++->(g: G[ND, ED], edges: (ED, NodeDesignator, NodeDesignator)*): G[ND, ED] =
    edges.foldLeft(g) { (graph, edge) => this +-> (graph, edge._1, edge._2, edge._3)}

  def -(g: G[ND, ED], nodeDes: NodeDesignator): G[ND, ED] = removeNode(g, nodeDes)
  def -->(g: G[ND, ED], edgeDes: EdgeDesignator): G[ND, ED] = removeEdge(g, edgeDes)

  /**
   * Dodaje do grafu inny graf. Dodanie odbywa sie na zasadzie sumowania zbiorów wezlow i krawedzi, przy
   * czym obowiazuje zasada, ze o tozsamosci elementu decyduja jego dane. Oznacza to, ze jesli w dodawanym
   * grafie wystepuja wezly z danymi, ktore wystepuja takze w wezlach tego grafu lub wystepuja krawedzie
   * z danymi, ktore wystepuja takze w krawedziach tego grafu, to odpowiednie elementy nie zostana dodane do tego
   * grafu. Nie wystapia wiec powtorzenia danych w ramach wezlow lub krawedzi (o ile nie bylo ich wczesniej w tym grafie).
   * Umozliwia to sensowne dodawanie grafow zgodnie z teoriomnogosciowymi intuicjami.
   */
  def ++(g: G[ND, ED], other: Graph[ND, ED]): G[ND, ED] = {
    // adding nodes - each node from other grah is added only when data of
    // this node does not exists in intermediate graph. For this reason,
    // possible data duplication from other graph are removed (but not these from
    // this graph)
    val new_graph_2 = other.nodes.foldLeft(g) {(graph, node) => graph.node(node.Data.da) match {
      case Some(ex_node) => graph
      case _ => this + (graph, node.Data) }}

    // adding edges - each edge of other graph is added only when data of this
    // edge does not exists in intermediate graph. For this reason,
    // possible data duplication from other graph are removed (but not these
    // from these graph)
    other.edges.foldLeft(new_graph_2) {(graph, edge) => graph.edge(edge.Data.eda) match {
      case Some(ex_node) => graph
      case _ => this +-> (graph, edge.Data, edge.SrcNode, edge.DstNode)}
    }
  }

  /**
   * Adds another graph to this graph not caring about data duplicates, so
   * effectively it creates a component in this graph containing exactly the
   * other graph.
   */
  def +++(g: G[ND, ED], other: Graph[ND, ED]): G[ND, ED] = {
    val nodes_map = Map[NodeIDDesignator, NodeIDDesignator]()
    // adding nodes and building node-node map in modified this and
    // other graph
    val (new_graph, nodes_map_2) = other.nodes.foldLeft[(G[ND, ED], Map[NodeIDDesignator, NodeIDDesignator])]((g, nodes_map)) {
      case ((graph, nodes_map), node) => (this + (graph, node.Data), nodes_map + (node.ID.id -> g.node((g.nodeCount - 1).i).get.ID.id))
    }

    other.edges.foldLeft[G[ND, ED]](new_graph) {(graph, edge) =>
      val inc_nodes = other.incident(edge)
      val node_des_col = List(inc_nodes._1, inc_nodes._2).map {(ni: NodeInfo[ND]) => nodes_map_2(ni.ID.id)}
      this +-> (graph, edge.Data, node_des_col(0), node_des_col(1))
    }
  }

  /**
   * Odejmuje graf od grafu reprezentowanego przez tę instancję. W zależności od implementacji metod
   * {@link removeNode()} i {@link removeEdge()} może działać zarówno w sposób mutowalny (w którym
   * odejmowanie dokonywane jest na mutowalnej bieżącej instancji i zwracana jest bieżąca instancja)
   * albo w sposób niemutowalny, w którym odejmowanie następuje na kopii bieżącej instancji.
   */
  def --(g: G[ND, ED], other: Graph[ND, ED]): G[ND, ED] = {
    // usuniecie węzłów z wynikowego grafu
    val new_graph_2 = other.nodes.foldLeft(g) { (graph, node) => this - (graph, node.Data.da)}
    // usunicie krawędzi z wynikowego grafu
    other.edges.foldLeft(new_graph_2) { (graph, edge) => this --> (graph, edge.Data.eda)}
  }

  /**
   * Uaktualnia dane wezla. Wlasciwa implementacja powinna byc zawarta w klasie
   * implementujacej. Powinno byc zachowane id wezla.
   */
  def updateNode(NodeDes: NodeDesignator, NewData: ND): G[ND, ED] =
    throw new Exception("Method unimplemented.")

  /**
   * Uaktualnia dane krawedzi. Wlasciwa implementacja powinna byc zawarta w klasie
   * implementujacej. Powinno byc zachowane id krawedzi.
   */
  def updateEdge(EdgeDes: EdgeDesignator, NewData: ED): G[ND, ED] =
    throw new Exception("Method unimplemented.")

  /**
   * Zmienia przeciwlegly wzgledem danego wezla wezel krawedzi. Powinny byc
   * zachowane id krawedzi i wezlow
   */
  def chngOppNode(nodeDes: NodeDesignator, edgeDes: EdgeDesignator,
                  newOppNodeDes: NodeDesignator): G[ND, ED] =
    throw new Exception("Method unimplemented.")

  /**
   * Adds a node and an edge joining source node with newly added node.
   */
  def joinNode(g: G[ND, ED], srcNode: NodeDesignator, nodeData: ND, edgeData: ED): G[ND, ED] = {
    val g2 = addNode(g, nodeData)
    addEdge(g2, edgeData, srcNode, g.nodeCount.i)
  }

  /**
   * Adds another graph as in +++ and joins it with this graph by an edge.
   */
  def joinGraph(g: G[ND, ED], srcNode: NodeDesignator, dstNode: NodeDesignator, edgeData: ED,
                otherGraph: Graph[ND, ED]): G[ND, ED] = ???
}
