package skac.euler.impl.fastindex.mutable

import skac.euler._
import skac.euler.General._
import skac.euler.impl.fastindex.generic._

//case class NodeStruct[ND, ED](var NodeInfo: NodeInfo[ND], var InEdges : Map[Any, EdgeInfo[ED]],
//                        var OutEdges: Map[Any, EdgeInfo[ED]])
//                        extends skac.euler.impl.generic.NodeStruct[ND, ED] {
//  def nodeInfo = NodeInfo
//  def inEdges = InEdges
//  def outEdges = OutEdges
//}

/**
 * Podstawowa implementacja mutowalna grafu zoptymalizowana pod kątem szybkiego dostępu
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
class Graph[ND, ED] extends AbstractGraph[ND, ED] {
  import skac.euler.General._

  type G = Graph[ND, ED]
  type NodesType = scala.collection.mutable.ArrayBuffer[NodeStruct[ND, ED]]
  type ConcreteNodeStruct = NodeStruct[ND, ED]

  /**
   * ArrayBuffer ze względu na stały czas dostępu po indeksie, amortyzowany
   * stały czas dodawania elementu (append) oraz mutowalność
   */
  override protected val Nodes = scala.collection.mutable.ArrayBuffer[NodeStruct[ND, ED]]()

  //type GraphElID = Int
  var NewNodeID = 0
  var NewEdgeID = 0

  protected def newNodeID: Int = {
    NewNodeID += 1
    NewNodeID - 1
  }

  protected def newEdgeID: Int = {
    NewEdgeID += 1
    NewEdgeID - 1
  }

  /**
   * Zwraca obiekt klasy NodeInfo na podstawie danych węzła. Nadpisanie metody
   * w klasie potomnej umożliwia wykorzystanie własnej podklasy klasy NodeInfo
   * w wewnętrznych strukturach danych reprezentujących węzły w celu
   * reprezentacji bogatszej informacji o węzłach (wag, nazw itp.).
   */
  def newNodeInfo(Data: ND): NodeInfo[ND] = new NodeInfo(newNodeID, Data)

  override protected def findNodeStruct(NodeDes: NodeDesignator): Option[ConcreteNodeStruct] =
    super.findNodeStruct(NodeDes).asInstanceOf[Option[ConcreteNodeStruct]]

  /**
   * Zwraca obiekt klasy EdgeInfo na podstawie danych krawędzi oraz desygnatorów
   * węzła źródłowego i docelowego. Nadpisanie metody
   * w klasie potomnej umożliwia wykorzystanie własnej podklasy klasy EdgeInfo
   * w wewnętrznych strukturach danych reprezentujących krawędzie w celu
   * reprezentacji bogatszej informacji o krawędziach (wag, nazw itp.).
   */
  def newEdgeInfo(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator): EdgeInfo[ED] =
    new EdgeInfo(newEdgeID, Data, SrcNode, DstNode)

  def addNode(Data: ND): Graph[ND, ED] = {
    Nodes.append(NodeStruct(newNodeInfo(Data), Map[Any, EdgeInfo[ED]](), Map[Any, EdgeInfo[ED]]()))
    this
  }

  def addEdge(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator) = {
    //val out_edges_count = Nodes(node(SrcNode).get.Index).OutEdges.size
    //val edge_id = newEdgeID
    //val edge_idx = edges.size
    // ze względu na to, że indeksy węzłów oraz ich dane nie są w ogólności niezmienne, trzeba w tworzonej strukturze
    // EdgeInfo umieścić desygnatory węzłów klasy NodeIDDesignator
    val src_node_id_des = NodeIDDesignator(node(SrcNode).get.ID)
    val dst_node_id_des = NodeIDDesignator(node(DstNode).get.ID)
    //val edge_info = new EdgeInfo(edge_id, Data, src_node_id_des, dst_node_id_des)
    val edge_info = newEdgeInfo(Data, src_node_id_des, dst_node_id_des)
    val src_ns = findNodeStruct(SrcNode).get
    val old_out_edges = src_ns.outEdges;
    // dodanie mapowania odpowiadającego krawędzi wychodzącej
    src_ns.outEdges = old_out_edges + (edge_info.ID -> edge_info)
    val dst_ns = findNodeStruct(DstNode).get
    val old_in_edges = dst_ns.inEdges;
    // dodanie mapowania odpowiadającego krawędzi wchodzącej
    dst_ns.inEdges = old_in_edges + (edge_info.ID -> edge_info)
    this
  }

  /**
   * Usuwa węzeł lub węzły. W przypadku, gdy desygnator jest typu data, usuwa wszystkie węzły z
   * określonymi danymi. W przypadku, gdy desygnator jest typu predykat, usuwa wszystkie węzły
   * spełniające określony predykat.
   */
  override def removeNode(NodeDes: NodeDesignator): Graph[ND, ED] = {
    edgesOfNode(NodeDes) foreach {ei: EdgeInfo[_] => removeEdge(ei.ID.eid)}
    NodeDes match {
      case NodeIDDesignator(id) => Nodes -= (Nodes find {_.nodeInfo.ID == id} get)
      case NodeIdxDesignator(idx) => Nodes remove idx
      // usuwa TYLKO 1 WEZEL z dana data
      case NodeDataDesignator(data: ND) => Nodes --= (nodesOf(data) map {(node) => {Nodes find {_.nodeInfo == node} get}})
      case NodePredDesignator(pred) => Nodes --= (Nodes filter {ns => pred(ns.nodeInfo)})
    }
    this
  }

  override def removeEdge(EdgeDes: EdgeDesignator) = {
    val edge_id = edge(EdgeDes).get.ID
    //val edge_id = EdgeDes.getID
    Nodes find (node_struct => {
      if (node_struct.outEdges.contains(edge_id)) {
        val edge_info = node_struct.outEdges(edge_id)
        // usuniecie info o krawedzi ze struktury drugiego wezla (koncowego wezla krawedzi)
        val end_node_struct = findNodeStruct(edge_info.DstNode).get.asInstanceOf[NodeStruct[ND, ED]]
        val in_edges = end_node_struct.inEdges
        end_node_struct.inEdges = in_edges - edge_id
        // usuniecie info o krawedzi wychodzacej
        node_struct.asInstanceOf[NodeStruct[ND, ED]].outEdges = node_struct.outEdges - edge_id
        true
      }
      else
        false
    })
    this
  }

  /**
   * Zastępuje bieżącą strukturę NodeInfo dla wybranego węzła w wewnętrznej reprezentacji
   * węzłów inną strukturą NodeInfo, tworzoną przez podaną funkcję na postawie starej struktury.
   * Wykorzystanie metody w klasie potomnej umożliwia
   * łatwe wykorzystanie podklas klasy NodeInfo w wewnętrznej reprezentacji węzłów
   * w celu reprezentacji bogatszej informacji o węzłach (np. wag, nazw itd.)
   */
  protected def replaceNodeInfo(NodeDes: NodeDesignator, ReplaceFun: General.NodeInfo[ND] => General.NodeInfo[ND]) {
    val n = node(NodeDes).get
    findNodeStruct(NodeDes).get.asInstanceOf[NodeStruct[ND, ED]].nodeInfo = ReplaceFun(n)
  }

  /**
   * Zastępuje bieżącą strukturę EdgeInfo dla wybranej krawędzi w wewnętrznej reprezentacji
   * krawędzi inną strukturą EdgeInfo tworzoną przez podaną funkcję na podstawie starej struktury.
   * Wykorzystanie metody w klasie potomnej umożliwia
   * łatwe wykorzystanie podklas klasy EdgeInfo w wewnętrznej reprezentacji krawędzi
   * w celu reprezentacji bogatszej informacji o krawędziach (np. wag, nazw itd.)
   */
  protected def replaceEdgeInfo(EdgeDes: EdgeDesignator, ReplaceFun: General.EdgeInfo[ED] => General.EdgeInfo[ED]) = {
    // krawędź, której waga jest modyfikowana
    val e = edge(EdgeDes).get
    // NodeStruct węzła źródłowego modyfikowanej krawędzi
    val src_ns = findNodeStruct(e.SrcNode).get.asInstanceOf[NodeStruct[ND, ED]]
    // zastąpienie starego NodeInfo nowym w mapie węzłów wyjściowych węzła źródłowegoe
    //src_ns.OutEdges = src_ns.OutEdges - e.ID + Tuple2(e.ID, ReplaceFun(e))
    src_ns.outEdges = src_ns.outEdges - e.ID + (e.ID -> ReplaceFun(e))
    src_ns.outEdges = src_ns.outEdges - e.ID + (e.ID -> ReplaceFun(e))
    // NodeStruct węzła docelowego modyfikowanej krawędzi
    val dst_ns = findNodeStruct(e.DstNode).get.asInstanceOf[NodeStruct[ND, ED]]
    // zastąpienie starego NodeInfo nowym w mapie węzłów wejściowych węzła docelowego
    //dst_ns.InEdges = dst_ns.InEdges - e.ID + Tuple2(e.ID, ReplaceFun(e))
    dst_ns.inEdges = dst_ns.inEdges - e.ID + (e.ID -> ReplaceFun(e))
  }

  override def updateNode(NodeDes: NodeDesignator, NewData: ND) = {
    val ns = findNodeStruct(NodeDes).get
    val old_ni = ns.nodeInfo
    ns.nodeInfo = new NodeInfo(old_ni.ID, NewData)
    this
  }
}
