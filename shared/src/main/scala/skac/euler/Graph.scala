package skac.euler

import General._
import scala.collection.generic._

trait Graph[ND, ED] extends GraphView[ND, ED] {
  import General._
  import GraphView._

  def nodeCount: Int
  def edgeCount: Int

  /**
   * Zwraca węzły zawierające określone dane.
   */
  def nodesOf(Data: ND): NodeInfos = nodes filter {_.Data == Data} toSet

  /**
   * Zwraca zbór krawędzi zawierających określone dane.
   */
  def edgesOf(Data: ED): EdgeInfos = edges filter {_.Data == Data} toSet

  def nodeForIdx(Index: Int): Option[NodeInfo[ND]] = node(NodeIdxDesignator(Index))

  def edgeForIdx(Index: Int): Option[EdgeInfo[ED]] = edge(EdgeIdxDesignator(Index))

  /**
   * Returns all edges of a graph.
   */
  def edges: Iterable[EdgeInfo[ED]] = (0 until edgeCount).view map {edgeForIdx(_).get}

  override def edges(nd: NodeDesignator) = edges filter { isEdgeOf(_, nd) } toSet

  override def edges(nd: NodeDesignator, direction: Int =
   NEIGHBOR_SIDE_BOTH) = edges filter { isEdgeOf(_, nd, direction)} toSet

  override def inEdges(nd: NodeDesignator) = edges filter (isInEdgeOfNode(_, nd)) toSet

  override def outEdges(nd: NodeDesignator) = edges filter (isOutEdgeOfNode(_, nd)) toSet

  /**
   * Returns all nodes of a graph.
   */
  def nodes: Iterable[NodeInfo[ND]] = (0 until nodeCount).view map {nodeForIdx(_).get}

  private def isEdgeOf(ei: EdgeInfo[ED], nd: NodeDesignator) =
   ei.SrcNode === nd || ei.DstNode === nd

  private def isEdgeOf(ei: EdgeInfo[ED], nd: NodeDesignator, direction: Int) =
   direction match {
    case NEIGHBOR_SIDE_FORWARD => ei.SrcNode === nd
    case NEIGHBOR_SIDE_BACKWARD => ei.DstNode === nd
    case NEIGHBOR_SIDE_BOTH => ei.SrcNode === nd || ei.DstNode === nd
  }

  protected val isInEdgeOfNode = (edge: EdgeInfo[ED], nd: NodeDesignator) =>
   edge.DstNode === nd

  protected val isOutEdgeOfNode = (edge: EdgeInfo[ED], nd: NodeDesignator) =>
    edge.SrcNode === nd

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
  def ++(other: Graph[ND, ED]): Graph[ND, ED] = {
    // adding nodes - each node from other grah is added only when data of
    // this node does not exists in intermediate graph. For this reason,
    // possible data duplication from other graph are removed (but not these from
    // this graph)
    val new_graph_2 = other.nodes.foldLeft(this) {(graph, node) => graph.node(node.Data) match {
      case Some(ex_node) => graph
      case _ => graph + node.Data
    }
    // adding edges - each edge of other graph is added only when data of this
    // edge does not exists in intermediate graph. For this reason,
    // possible data duplication from other graph are removed (but nod these
    // from these graph)
    other.edges.foldLeft(new_graph_2) {(graph, edge) => graph.edge(edge.Data) match {
      case Some(ex_node) => graph
      case _ => graph +-> (edge.Data, edge.SrcNode, edge.DstNode)}
    }
  }

  /**
   * Adds another graph to this graph not caring about data duplicates, so
   * effectively it creates a component in this graph containing exactly the
   * other graph.
   */
  def +++(other: Graph[ND, ED]): Graph[ND, ED] = {
    val nodes_map = Map[NodeIDDesignator, NodeIDDesignator]()
    // adding nodes and building node-node map in modified this and
    // other node
    val (new_graph, nodes_map_2) = other.nodes.foldLeft((this, nodes_map)) {(kv, node) =>
     (graph + node.Data, nodes_map + (node.ID.id -> this.node((this.nodeCount - 1).i).get.ID.id))}

    other.edges.foldLeft(new_graph) {(graph, edge) => }
    ...
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
   * Displays some info about graph.
   */
  def about() {
    println(s"Basic graph characteristics: \r\nNode count: ${nodeCount}, edge count: ${edgeCount}.")
  }

  /**
   * Adds a node and an edge joining source node with newly added node.
   */
  def joinNode(srcNode: NodeDesigator, nodeData: ND, edgeData: ED): Graph[ND, ED] = {
    val node_count = addNode(nodeData).nodeCount
    addEdge(edgeData, srcNode, node_count.i)
  }

  /**
   * Adds another graph as in +++ and joins it with this graph by an edge.
   */
  def joinGraph(srcNode: NodeDesignator, dstNode: NodeDesignator, edgeData: ED,
   otherGraph: Graph[ND, ED]): Graph[ND, ED] = ???

}
