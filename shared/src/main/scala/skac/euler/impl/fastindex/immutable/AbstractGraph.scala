package skac.euler.impl.fastindex.immutable

import skac.euler._

/**
 * Podstawowa implementacja niemutowalna grafu zoptymalizowana pod kątem szybkiego dostępu
 * do węzłów na podstawie indeksów oraz szybkiego dostępu do krawędzi
 * danego węzła. Informacja o elementach grafu zapisana jest w ArrayBuffer
 * {@link Nodes}. Elementami tej struktury są obiekty klasy NodeStruct. Każdy
 * obiekt klasy NodeStruct przechowuje informacje o pojedynczym węźle oraz
 * wszystkich krawędziach przyległych. Dzięki temu uzyskuje się szybki dostępdef
 * do węzłów na podstawie indeksów oraz szybki dostęp do krawędzi danego węzła.
 * Klasa umożliwia łatwe rozszerzenie z możliwością zastąpienia klas NodeInfo i
 * EdgeInfo ich rozszerzeniami w wewnętrznych obiektach reprezentujących
 * strukturę grafu. W tym celu zdefiniowane są metody {@link newNodeInfo()},
 * {@link newEdgeInfo()}, {@link replaceNodeInfo()} oraz {@link replaceEdgeInfo()}.
 */

object AbstractGraph {
  implicit def modifier[G[ND, ED] <: AbstractGraph[G[ND, ED], ND, ED], ND, ED] = new GraphModifier[G, ND, ED] {
    override def addNode(g: G[ND, ED], data: ND): G[ND, ED] = {
      val new_nodes: Vector[NodeStruct[ND, ED]] = g.Nodes :+ NodeStruct(g.newNodeInfo(data), Map[Any, EdgeInfo[ED]](),
        Map[Any, EdgeInfo[ED]]())
      g.newInstance(new_nodes)
    }

    override def addEdge(g: G[ND, ED], data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator): G [ND, ED]= {
      // w kolekcji Nodes podmieniane są elementy klasy ConcreteNodeStruct dla źródłowego i docelowego
      // węzła. W węźle źródłowym mapa OutEdges różni się od pierwotnej dodaniem pozycji
      // odpowiadającej dodawanej krawędzi. Analogiczna zmiana dotyczy węzła docelowego, z tym, że
      // dotyczy mapy InEdges
      val src_node_id_des = NodeIDDesignator(g.node(srcNode).get.ID)
      val dst_node_id_des = NodeIDDesignator(g.node(dstNode).get.ID)
      val edge_info = g.newEdgeInfo(data, src_node_id_des, dst_node_id_des)
      val src_node_struct = g.findNodeStruct(srcNode).get.asInstanceOf[NodeStruct[ND, ED]]
      val dst_node_struct = g.findNodeStruct(dstNode).get.asInstanceOf[NodeStruct[ND, ED]]
      val new_out_edges = src_node_struct.OutEdges + (edge_info.ID -> edge_info)
      val new_src_node_struct = src_node_struct.copy(OutEdges = new_out_edges).asInstanceOf[NodeStruct[ND, ED]]
      val new_in_edges = dst_node_struct.InEdges + (edge_info.ID -> edge_info)
      val new_dst_node_struct = dst_node_struct.copy(InEdges = new_in_edges).asInstanceOf[NodeStruct[ND, ED]]

      val new_nodes = ((src_node_struct, new_src_node_struct) ::
        (dst_node_struct, new_dst_node_struct) :: Nil).foldLeft(g.Nodes) {(n, pair) => {
        n updated (n.indexOf(pair._1), pair._2)}}

      g.newInstance(new_nodes)
    }

    override def removeNode(g: G[ND, ED], nodeDes: NodeDesignator): G[ND, ED] = {
      // usuniecie krawędzi wchodzących do usuwanego węzła - krawędzie wychodzące zostaną usunięte
      // automatycznie jako konsekwencja usunięcia struktury związanej z usuwanym węzłem
      val res = g.inEdges(nodeDes).foldLeft(g) { (g, e) => removeEdge(g, e)}
      val node_struct = res.findNodeStruct(nodeDes)
      val new_nodes = res.Nodes filter {_ != node_struct.get}
      g.newInstance(new_nodes)
    }

    override def removeEdge(g: G[ND, ED], edgeDes: EdgeDesignator): G[ND, ED] = {
      // w kolekcji Nodes podmieniane są elementy klasy NodeStruct dla źródłowego i docelowego
      // węzła. W każdej z tych 2 instancji struktur podmieniane są mapy krawędzi wchodzących i wychodzących
      // poprzez usunięcie (w stosunku do pierwotnych map) pozycji odpowiadających usuwanej krawędzi
      val e = g.edge(edgeDes).get
      val edge_id = e.ID
      val src_node_struct = g.findNodeStruct(e.SrcNode).get.asInstanceOf[NodeStruct[ND, ED]]
      val dst_node_struct = g.findNodeStruct(e.DstNode).get.asInstanceOf[NodeStruct[ND, ED]]

      val new_nodes = (src_node_struct :: dst_node_struct :: Nil).foldLeft(g.Nodes) {(n, ns) =>
      {n updated (n.indexOf(ns), ns.copy(InEdges = ns.InEdges - e.ID, OutEdges = ns.OutEdges - e.ID).asInstanceOf[NodeStruct[ND, ED]])}}

      g.newInstance(new_nodes)
    }
  }
}

abstract class AbstractGraph[+G <: AbstractGraph[G, ND, ED], ND, ED](pNodes: Vector[NodeStruct[ND, ED]] = Vector[NodeStruct[ND, ED]](),
                                                                    newElemIdSeed: Option[Int] = None)
  extends skac.euler.impl.fastindex.generic.AbstractGraph[ND, ED] {

//   type G = this.type
//  type G = Graph[ND, ED]

  def this() {
    this(Vector())
  }

  override val Nodes = pNodes
  override type NodesT = Vector[NodeStruct[ND, ED]]
  private lazy val newNodeId = newElemIdSeed.getOrElse(0)
  private lazy val newEdgeId = newNodeId
  protected lazy val newNewElemIdSeed = Some(newElemIdSeed.getOrElse(0) + 1)

  /**
   * Zwraca obiekt klasy NodeInfo na podstawie danych węzła. Nadpisanie metody
   * w klasie potomnej umożliwia wykorzystanie własnej podklasy klasy NodeInfo
   * w wewnętrznych strukturach danych reprezentujących węzły w celu
   * reprezentacji bogatszej informacji o węzłach (wag, nazw itp.).
   */
  def newNodeInfo(data: ND): NodeInfo[ND] = new NodeInfo(newNodeId, data)

  /**
   * Zwraca obiekt klasy EdgeInfo na podstawie danych krawędzi oraz desygnatorów
   * węzła źródłowego i docelowego. Nadpisanie metody
   * w klasie potomnej umożliwia wykorzystanie własnej podklasy klasy EdgeInfo
   * w wewnętrznych strukturach danych reprezentujących krawędzie w celu
   * reprezentacji bogatszej informacji o krawędziach (wag, nazw itp.).
   */
  def newEdgeInfo(data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator): EdgeInfo[ED] =
    new EdgeInfo(newEdgeId, data, srcNode, dstNode)

//  override def addNode(data: ND): G = {
//    val new_nodes: Vector[NodeStruct[ND, ED]] = Nodes :+ NodeStruct(newNodeInfo(data), Map[Any, EdgeInfo[ED]](),
//      Map[Any, EdgeInfo[ED]]())
//    newInstance(new_nodes)
//  }

  /**
   * Zwraca kolekcję Nodes, w której w stosunku do kolekcji tego grafu zmieniona jest tylko jedna
   * instancja NodeInfo w jednym z obiektów NodeStruct
   */
  def replaceNodeInfo(nodeDes: NodeDesignator, replaceFun: (NodeInfo[ND]) => NodeInfo[ND]) = {
    val node_struct = findNodeStruct(nodeDes).get.asInstanceOf[NodeStruct[ND, ED]]
    val old_node_info = node_struct.nodeInfo.asInstanceOf[NodeInfo[ND]]
    val new_node_struct = node_struct.copy(NodeInfo = replaceFun(node_struct.NodeInfo.asInstanceOf[NodeInfo[ND]])).asInstanceOf[NodeStruct[ND, ED]]
    val new_nodes = Nodes updated (Nodes.indexOf(node_struct), new_node_struct)
    new_nodes
  }

  /**
   * Zwraca kolekcję Nodes, w której w stosunku do tego kolekcji grafu zmienione są tylko instancje
   * EdgeInfo w 2 strukturach NodeStruct (odpowiadających węzłowi źródłowemu i docelowemu)
   */
  protected def replaceEdgeInfo(edgeDes: EdgeDesignator, replaceFun: (EdgeInfo[ED]) => EdgeInfo[ED]) = {
    val e = edge(edgeDes).get.asInstanceOf[EdgeInfo[ED]]
    val edge_id = e.ID
    val src_node_struct = findNodeStruct(e.SrcNode).get.asInstanceOf[NodeStruct[ND, ED]]
    val dst_node_struct = findNodeStruct(e.DstNode).get.asInstanceOf[NodeStruct[ND, ED]]

    val new_nodes = (src_node_struct :: dst_node_struct :: Nil).foldLeft(Nodes) {(n, ns) =>
      {n updated (n.indexOf(ns), ns.copy(InEdges = ns.InEdges + (e.ID -> replaceFun(e)),
                                         OutEdges = ns.OutEdges + (e.ID -> replaceFun(e))))}}

    new_nodes
  }

//  def addEdge(data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator): G = {
//    // w kolekcji Nodes podmieniane są elementy klasy ConcreteNodeStruct dla źródłowego i docelowego
//    // węzła. W węźle źródłowym mapa OutEdges różni się od pierwotnej dodaniem pozycji
//    // odpowiadającej dodawanej krawędzi. Analogiczna zmiana dotyczy węzła docelowego, z tym, że
//    // dotyczy mapy InEdges
//    val src_node_id_des = NodeIDDesignator(node(srcNode).get.ID)
//    val dst_node_id_des = NodeIDDesignator(node(dstNode).get.ID)
//    val edge_info = newEdgeInfo(data, src_node_id_des, dst_node_id_des)
//    val src_node_struct = findNodeStruct(srcNode).get.asInstanceOf[NodeStruct[ND, ED]]
//    val dst_node_struct = findNodeStruct(dstNode).get.asInstanceOf[NodeStruct[ND, ED]]
//    val new_out_edges = src_node_struct.OutEdges + (edge_info.ID -> edge_info)
//    val new_src_node_struct = src_node_struct.copy(OutEdges = new_out_edges).asInstanceOf[NodeStruct[ND, ED]]
//    val new_in_edges = dst_node_struct.InEdges + (edge_info.ID -> edge_info)
//    val new_dst_node_struct = dst_node_struct.copy(InEdges = new_in_edges).asInstanceOf[NodeStruct[ND, ED]]
//
//    val new_nodes = ((src_node_struct, new_src_node_struct) ::
//                     (dst_node_struct, new_dst_node_struct) :: Nil).foldLeft(Nodes) {(n, pair) => {
//          n updated (n.indexOf(pair._1), pair._2)}}
//
//    newInstance(new_nodes)
//  }

  /**
   * Usuwa węzeł lub węzły. W przypadku, gdy desygnator jest typu data, usuwa wszystkie węzły z
   * określonymi danymi. W przypadku, gdy desygnator jest typu predykat, usuwa wszystkie węzły
   * spełniające określony predykat.
   */
//  def removeNode(nodeDes: NodeDesignator): G = {
//    // usuniecie krawędzi wchodzących do usuwanego węzła - krawędzie wychodzące zostaną usunięte
//    // automatycznie jako konsekwencja usunięcia struktury związanej z usuwanym węzłem
//    val res = inEdges(nodeDes).foldLeft(this) { (g, e) => g.removeEdge(e)}
//    val node_struct = res.findNodeStruct(nodeDes)
//    val new_nodes = res.Nodes filter {_ != node_struct.get}
//    newInstance(new_nodes)
//  }

//  def removeEdge(edgeDes: EdgeDesignator) = {
//    // w kolekcji Nodes podmieniane są elementy klasy NodeStruct dla źródłowego i docelowego
//    // węzła. W każdej z tych 2 instancji struktur podmieniane są mapy krawędzi wchodzących i wychodzących
//    // poprzez usunięcie (w stosunku do pierwotnych map) pozycji odpowiadających usuwanej krawędzi
//    val e = edge(edgeDes).get
//    val edge_id = e.ID
//    val src_node_struct = findNodeStruct(e.SrcNode).get.asInstanceOf[NodeStruct[ND, ED]]
//    val dst_node_struct = findNodeStruct(e.DstNode).get.asInstanceOf[NodeStruct[ND, ED]]
//
//    val new_nodes = (src_node_struct :: dst_node_struct :: Nil).foldLeft(Nodes) {(n, ns) =>
//      {n updated (n.indexOf(ns), ns.copy(InEdges = ns.InEdges - e.ID, OutEdges = ns.OutEdges - e.ID).asInstanceOf[NodeStruct[ND, ED]])}}
//
//    newInstance(new_nodes)
//  }

  protected def newInstance(nodes: Vector[NodeStruct[ND, ED]]): G
//    new CustomGraph[G, ND, ED](Nodes, newNewElemIdSeed).asInstanceOf[G]
}
