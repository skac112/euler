package skac.euler
import cats.Id

object AutoModifiableGraph {
  implicit def gTomg[G[ND, ED] <: Graph[ND, ED], ND, ED](g: G[ND, ED])(implicit m: GraphModifier[G, ND, ED])= AutoModifiableGraph(g)(m)
  implicit def mgTog[G[ND, ED] <: Graph[ND, ED], ND, ED](mg: AutoModifiableGraph[G, ND, ED]) = mg.g
}

import AutoModifiableGraph._

case class AutoModifiableGraph[G[ND, ED] <: Graph[ND, ED], ND, ED](g: G[ND, ED])(implicit m: GraphModifier[G, ND, ED])
  extends ModifiableGraph[AutoModifiableGraph[G, ND, ED], ND, ED] {
  override def addNode(data: ND) = gTomg(m.addNode(g, data))
  override def addEdge(data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator) = gTomg(m.addEdge(g, data, srcNode, dstNode))
  override def removeNode(nodeDes: NodeDesignator) = m.removeNode(g, nodeDes)
  override def removeEdge(edgeDes: EdgeDesignator) = m.removeEdge(g, edgeDes)
  override def nodeCount: Int = g.nodeCount
  override def edgeCount: Int = g.edgeCount
  override def node[SND >: ND](nodeDes: NodeDesignator): Option[NodeInfo[SND]] = g.node(nodeDes)
  override def edge[SED >: ED](edgeDes: EdgeDesignator): Option[EdgeInfo[SED]] = g.edge(edgeDes)
}

//trait ModifiableGraph[+G <: ModifiableGraph[G, ND, ED], ND, ED] extends Graph[G, ND, ED]{
//  def addNode(data: ND): G
//  def addEdge(data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator): G
//  def removeNode(nodeDes: NodeDesignator): G
//  def removeNode(nodeData: ND): G = removeNode(NodeDataDesignator(nodeData))
//  def removeEdge(edgeDes: EdgeDesignator): G
//  def removeEdge(edgeData: ED): G = removeEdge(EdgeDataDesignator(edgeData))
//
//  def reverseEdge(edgeDes: EdgeDesignator): G = {
//    val e = edge(edgeDes).get
//    val g = removeEdge(edgeDes)
//    g.addEdge(e.Data, e.DstNode, e.SrcNode)
//  }
//
//  /**
//   * Usuwa wszystkie elementy z grafu. W zależności od implementacji metody {@link removeNode()} może
//   * działać w sposób mutowalny lub niemutowalny. Ze względu na niską wydajność tej implementacji,
//   * wskazane jest, aby klasa implementująca nadpisywała tę metodę usuwając dane elementów w sposób
//   * specyficzny dla ich wewnętrznej reprezentacji w danej klasie
//   */
//  def clear: G = (1 to nodeCount).foldLeft(this.asInstanceOf[G]) {(g, idx) => g.removeNode(0.i)}
//
//
//  def +->(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator): G = addEdge(Data, SrcNode, DstNode)
//
//  //def +=(Nodes: Traversable[ND]): Graph[ND, ED] = Nodes.foldLeft(this) {(graph, node_data) => graph += node_data}
//
//  def ++(Nodes: ND*): G = Nodes.foldLeft[G](this.asInstanceOf[G]) {(graph, node_data) => graph + node_data}
//
//  //  def +=(Edges: Traversable[Tuple3[ED, NodeDesignator, NodeDesignator]]): Graph[ND, ED] = Edges.foldLeft(this) {
//  //    (graph, edge) => graph += (edge._1, edge._2, edge._3)}
//
//  def ++->(Edges: (ED, NodeDesignator, NodeDesignator)*): G =
//    Edges.foldLeft[G](this.asInstanceOf[G]) { (graph, edge) => graph +-> (edge._1, edge._2, edge._3)}
//
//  def -(NodeDes: NodeDesignator): G = removeNode(NodeDes)
//  def -->(EdgeDes: EdgeDesignator): G = removeEdge(EdgeDes)
//
//  /**
//   * Dodaje do grafu inny graf. Dodanie odbywa sie na zasadzie sumowania zbiorów wezlow i krawedzi, przy
//   * czym obowiazuje zasada, ze o tozsamosci elementu decyduja jego dane. Oznacza to, ze jesli w dodawanym
//   * grafie wystepuja wezly z danymi, ktore wystepuja takze w wezlach tego grafu lub wystepuja krawedzie
//   * z danymi, ktore wystepuja takze w krawedziach tego grafu, to odpowiednie elementy nie zostana dodane do tego
//   * grafu. Nie wystapia wiec powtorzenia danych w ramach wezlow lub krawedzi (o ile nie bylo ich wczesniej w tym grafie).
//   * Umozliwia to sensowne dodawanie grafow zgodnie z teoriomnogosciowymi intuicjami.
//   */
//  def ++(other: Graph[_, ND, ED]): G = {
//    // adding nodes - each node from other grah is added only when data of
//    // this node does not exists in intermediate graph. For this reason,
//    // possible data duplication from other graph are removed (but not these from
//    // this graph)
//    val new_graph_2 = other.nodes.foldLeft[G](this.asInstanceOf[G]) {(graph, node) => graph.node(node.Data.da) match {
//      case Some(ex_node) => graph
//      case _ => graph + node.Data }}
//
//    // adding edges - each edge of other graph is added only when data of this
//    // edge does not exists in intermediate graph. For this reason,
//    // possible data duplication from other graph are removed (but not these
//    // from these graph)
//    other.edges.foldLeft(new_graph_2) {(graph, edge) => graph.edge(edge.Data.eda) match {
//      case Some(ex_node) => graph
//      case _ => graph +-> (edge.Data, edge.SrcNode, edge.DstNode)}
//    }
//  }
//
//  /**
//   * Adds another graph to this graph not caring about data duplicates, so
//   * effectively it creates a component in this graph containing exactly the
//   * other graph.
//   */
//  def +++(other: Graph[_, ND, ED]): G = {
//    val nodes_map = Map[NodeIDDesignator, NodeIDDesignator]()
//    // adding nodes and building node-node map in modified this and
//    // other graph
//    val (new_graph, nodes_map_2) = other.nodes.foldLeft[(G, Map[NodeIDDesignator, NodeIDDesignator])]((this.asInstanceOf[G], nodes_map)) {
//      case ((graph, nodes_map), node) => (graph + node.Data, nodes_map + (node.ID.id -> this.node((this.nodeCount - 1).i).get.ID.id))
//    }
//
//    other.edges.foldLeft[G](new_graph) {(graph, edge) =>
//      val inc_nodes = other.incident(edge)
//      val node_des_col = List(inc_nodes._1, inc_nodes._2).map {(ni: NodeInfo[ND]) => nodes_map_2(ni.ID.id)}
//      graph +-> (edge.Data, node_des_col(0), node_des_col(1))
//    }
//  }
//
//  /**
//   * Odejmuje graf od grafu reprezentowanego przez tę instancję. W zależności od implementacji metod
//   * {@link removeNode()} i {@link removeEdge()} może działać zarówno w sposób mutowalny (w którym
//   * odejmowanie dokonywane jest na mutowalnej bieżącej instancji i zwracana jest bieżąca instancja)
//   * albo w sposób niemutowalny, w którym odejmowanie następuje na kopii bieżącej instancji.
//   */
//  def --(other: Graph[_, ND, ED]): G = {
//    val new_graph_1 = this.asInstanceOf[G]
//    // usunicie węzłów z wynikowego grafu
//    val new_graph_2 = other.nodes.foldLeft(new_graph_1) { (graph, node) => graph - node.Data.da}
//    // usunicie krawędzi z wynikowego grafu
//    other.edges.foldLeft(new_graph_2) { (graph, edge) => graph --> edge.Data.eda}
//  }
//
//  /**
//   * Uaktualnia dane wezla. Wlasciwa implementacja powinna byc zawarta w klasie
//   * implementujacej. Powinno byc zachowane id wezla.
//   */
//  def updateNode(NodeDes: NodeDesignator, NewData: ND): G =
//    throw new Exception("Method unimplemented.")
//
//  /**
//   * Uaktualnia dane krawedzi. Wlasciwa implementacja powinna byc zawarta w klasie
//   * implementujacej. Powinno byc zachowane id krawedzi.
//   */
//  def updateEdge(EdgeDes: EdgeDesignator, NewData: ED): G =
//    throw new Exception("Method unimplemented.")
//
//  /**
//   * Zmienia przeciwlegly wzgledem danego wezla wezel krawedzi. Powinny byc
//   * zachowane id krawedzi i wezlow
//   */
//  def chngOppNode(nodeDes: NodeDesignator, edgeDes: EdgeDesignator,
//                  newOppNodeDes: NodeDesignator): G =
//    throw new Exception("Method unimplemented.")
//
//  /**
//   * Adds a node and an edge joining source node with newly added node.
//   */
//  def joinNode(srcNode: NodeDesignator, nodeData: ND, edgeData: ED): G = {
//    val g = addNode(nodeData)
//    addEdge(edgeData, srcNode, g.nodeCount.i)
//  }
//
//  /**
//   * Adds another graph as in +++ and joins it with this graph by an edge.
//   */
//  def joinGraph(srcNode: NodeDesignator, dstNode: NodeDesignator, edgeData: ED,
//                otherGraph: Graph[_, ND, ED]): G = ???
//}
