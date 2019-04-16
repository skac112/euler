package skac.euler.impl.fastindex.immutable

import skac.euler._
import skac.euler.General._
import skac.euler.impl.fastindex.generic.AbstractGraph

/**
 * Podstawowa implementacja niemutowalna grafu zoptymalizowana pod kątem szybkiego dostępu
 * do węzłów na podstawie indeksów oraz szybkiego dostępu do krawędzi
 * danego węzła. Informacja o elementach grafu zapisana jest w ArrayBuffer
 * {@link Nodes}. Elementami tej struktury są obiekty klasy NodeStruct. Każdy
 * obiekt klasy NodeStruct przechowuje informacje o pojedynczym węźle oraz
 * wszystkich krawędziach przyległych. Dzięki temu uzyskuje się szybki dostęp
 * do węzłów na podstawie indeksów oraz szybki dostęp do krawędzi danego węzła.
 * Klasa umożliwia łatwe rozszerzenie z możliwością zastąpienia klas NodeInfo i
 * EdgeInfo ich rozszerzeniami w wewnętrznych obiektach reprezentujących
 * strukturę grafu. W tym celu zdefiniowane są metody {@link newNodeInfo()},
 * {@link newEdgeInfo()}, {@link replaceNodeInfo()} oraz {@link replaceEdgeInfo()}.
 */
class Graph[ND, ED](pNodes: Vector[NodeStruct[ND, ED]] = Vector[NodeStruct[ND, ED]](),
                    newElemIdSeed: Option[Int] = None) extends AbstractGraph[ND, ED] {

  // type G = this.type
  type G = Graph[ND, ED]

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
  def newNodeInfo[SND >: ND](Data: SND): NodeInfo[SND] = new NodeInfo(newNodeId, Data)

  /**
   * Zwraca obiekt klasy EdgeInfo na podstawie danych krawędzi oraz desygnatorów
   * węzła źródłowego i docelowego. Nadpisanie metody
   * w klasie potomnej umożliwia wykorzystanie własnej podklasy klasy EdgeInfo
   * w wewnętrznych strukturach danych reprezentujących krawędzie w celu
   * reprezentacji bogatszej informacji o krawędziach (wag, nazw itp.).
   */
  def newEdgeInfo[SED >: ED](Data: SED, SrcNode: NodeDesignator, DstNode: NodeDesignator): EdgeInfo[SED] =
    new EdgeInfo(newEdgeId, Data, SrcNode, DstNode)

  def addNode[SND >: ND](Data: SND): Graph[SND, ED] = {
  // def addNode(Data: ND): G = {
    val new_nodes: Vector[NodeStruct[SND, ED]] = (Nodes :+ NodeStruct(newNodeInfo(Data), Map[Any, EdgeInfo[ED]](),
      Map[Any, EdgeInfo[ED]]()))
    newInstance(new_nodes)
  }

  /**
   * Zwraca kolekcję Nodes, w której w stosunku do kolekcji tego grafu zmieniona jest tylko jedna
   * instancja NodeInfo w jednym z obiektów NodeStruct
   */
  protected def replaceNodeInfo(NodeDes: NodeDesignator, ReplaceFun: (NodeInfo[ND]) => NodeInfo[ND]) = {
    val node_struct = findNodeStruct(NodeDes).get.asInstanceOf[NodeStruct[ND, ED]]
    val old_node_info = node_struct.nodeInfo.asInstanceOf[NodeInfo[ND]]
    val new_node_struct = node_struct.copy(NodeInfo = ReplaceFun(node_struct.NodeInfo.asInstanceOf[NodeInfo[ND]])).asInstanceOf[NodeStruct[ND, ED]]
    val new_nodes = Nodes updated (Nodes.indexOf(node_struct), new_node_struct)
    new_nodes
  }

  /**
   * Zwraca kolekcję Nodes, w której w stosunku do tego kolekcji grafu zmienione są tylko instancje
   * EdgeInfo w 2 strukturach NodeStruct (odpowiadających węzłowi źródłowemu i docelowemu)
   */
  protected def replaceEdgeInfo(EdgeDes: EdgeDesignator, ReplaceFun: (EdgeInfo[ED]) => EdgeInfo[ED]) = {
    val e = edge(EdgeDes).get.asInstanceOf[EdgeInfo[ED]]
    val edge_id = e.ID
    val src_node_struct = findNodeStruct(e.SrcNode).get.asInstanceOf[NodeStruct[ND, ED]]
    val dst_node_struct = findNodeStruct(e.DstNode).get.asInstanceOf[NodeStruct[ND, ED]]

    val new_nodes = (src_node_struct :: dst_node_struct :: Nil).foldLeft(Nodes) {(n, ns) =>
      {n updated (n.indexOf(ns), ns.copy(InEdges = ns.InEdges + (e.ID -> ReplaceFun(e)),
                                         OutEdges = ns.OutEdges + (e.ID -> ReplaceFun(e))))}}

    new_nodes
  }

  def addEdge[SED >: ED](Data: SED, SrcNode: NodeDesignator, DstNode: NodeDesignator): Graph[ND, SED] = {
    // w kolekcji Nodes podmieniane są elementy klasy ConcreteNodeStruct dla źródłowego i docelowego
    // węzła. W węźle źródłowym mapa OutEdges różni się od pierwotnej dodaniem pozycji
    // odpowiadającej dodawanej krawędzi. Analogiczna zmiana dotyczy węzła docelowego, z tym, że
    // dotyczy mapy InEdges
    val src_node_id_des = NodeIDDesignator(node(SrcNode).get.ID)
    val dst_node_id_des = NodeIDDesignator(node(DstNode).get.ID)
    val edge_info = newEdgeInfo(Data, src_node_id_des, dst_node_id_des)
    val src_node_struct = findNodeStruct(SrcNode).get.asInstanceOf[NodeStruct[ND, ED]]
    val dst_node_struct = findNodeStruct(DstNode).get.asInstanceOf[NodeStruct[ND, ED]]
    val new_out_edges = src_node_struct.OutEdges + (edge_info.ID -> edge_info)
    val new_src_node_struct = src_node_struct.copy(OutEdges = new_out_edges).asInstanceOf[NodeStruct[ND, ED]]
    val new_in_edges = dst_node_struct.InEdges + (edge_info.ID -> edge_info)
    val new_dst_node_struct = dst_node_struct.copy(InEdges = new_in_edges).asInstanceOf[NodeStruct[ND, ED]]

    val new_nodes = ((src_node_struct, new_src_node_struct) ::
                     (dst_node_struct, new_dst_node_struct) :: Nil).foldLeft(Nodes) {(n, pair) => {
          n updated (n.indexOf(pair._1), pair._2)}}

    newInstance(new_nodes)
  }

  /**
   * Usuwa węzeł lub węzły. W przypadku, gdy desygnator jest typu data, usuwa wszystkie węzły z
   * określonymi danymi. W przypadku, gdy desygnator jest typu predykat, usuwa wszystkie węzły
   * spełniające określony predykat.
   */
  def removeNode(NodeDes: NodeDesignator): Graph[ND, ED] = {
    // usuniecie wchodzących do usuwanego węzła - krawędzie wychodzące zostaną usunięte
    // automatycznie jako konsekwencja usunięcia struktury związanej z usuwanym węzłem
    val res = inEdges(NodeDes).foldLeft(this) {(g, e) => g.removeEdge(e)}
    val node_struct = findNodeStruct(NodeDes)
    val new_nodes = Nodes filter {_ != node_struct.get}
    newInstance(new_nodes)
  }

  def removeEdge(EdgeDes: EdgeDesignator) = {
    // w kolekcji Nodes podmieniane są elementy klasy NodeStruct dla źródłowego i docelowego
    // węzła. W każdej z tych 2 instancji struktur podmieniane są mapy krawędzi wchodzących i wychodzących
    // poprzez usunięcie (w stosunku do pierwotnych map) pozycji odpowiadających usuwanej krawędzi
    val e = edge(EdgeDes).get
    val edge_id = e.ID
    val src_node_struct = findNodeStruct(e.SrcNode).get.asInstanceOf[NodeStruct[ND, ED]]
    val dst_node_struct = findNodeStruct(e.DstNode).get.asInstanceOf[NodeStruct[ND, ED]]

    val new_nodes = (src_node_struct :: dst_node_struct :: Nil).foldLeft(Nodes) {(n, ns) =>
      {n updated (n.indexOf(ns), ns.copy(InEdges = ns.InEdges - e.ID, OutEdges = ns.OutEdges - e.ID).asInstanceOf[NodeStruct[ND, ED]])}}

    newInstance(new_nodes)
  }

  def newInstance[SND >: ND, SED >: ED](Nodes: Vector[NodeStruct[SND, SED]]) =
    new Graph[SND, SED](Nodes, newNewElemIdSeed)
}
