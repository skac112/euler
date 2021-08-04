package skac.euler.impl.fastindex.immutable

import skac.euler._

abstract class AbstractModifiableGraph[G <: AbstractModifiableGraph[G, ND, ED], ND, ED](pNodes: Vector[NodeStruct[ND, ED]] = Vector[NodeStruct[ND, ED]](),
                                                                     newElemIdSeed: Option[Int] = None)
  extends AbstractGraph[G, ND, ED](pNodes, newElemIdSeed) with ModifiableGraph[G, ND, ED] { self: G =>

  override def addNode(data: ND): G = {
    val new_nodes: Vector[NodeStruct[ND, ED]] = Nodes :+ NodeStruct(newNodeInfo(data), Map[Any, EdgeInfo[ED]](),
      Map[Any, EdgeInfo[ED]]())
    newInstance(new_nodes)
  }

  def addEdge(data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator): G = {
    // w kolekcji Nodes podmieniane są elementy klasy ConcreteNodeStruct dla źródłowego i docelowego
    // węzła. W węźle źródłowym mapa OutEdges różni się od pierwotnej dodaniem pozycji
    // odpowiadającej dodawanej krawędzi. Analogiczna zmiana dotyczy węzła docelowego, z tym, że
    // dotyczy mapy InEdges
    val src_node_id_des = NodeIDDesignator(node(srcNode).get.ID)
    val dst_node_id_des = NodeIDDesignator(node(dstNode).get.ID)
    val edge_info = newEdgeInfo(data, src_node_id_des, dst_node_id_des)
    val src_node_struct = findNodeStruct(srcNode).get.asInstanceOf[NodeStruct[ND, ED]]
    val dst_node_struct = findNodeStruct(dstNode).get.asInstanceOf[NodeStruct[ND, ED]]
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
  def removeNode(nodeDes: NodeDesignator): G = {
    // usuniecie krawędzi wchodzących do usuwanego węzła - krawędzie wychodzące zostaną usunięte
    // automatycznie jako konsekwencja usunięcia struktury związanej z usuwanym węzłem
    val res = inEdges(nodeDes).foldLeft[G](this) { (g, e) => g.removeEdge(e)}
    val node_struct = res.findNodeStruct(nodeDes)
    val new_nodes = res.Nodes filter {_ != node_struct.get}
    newInstance(new_nodes)
  }

  def removeEdge(edgeDes: EdgeDesignator) = {
      // w kolekcji Nodes podmieniane są elementy klasy NodeStruct dla źródłowego i docelowego
      // węzła. W każdej z tych 2 instancji struktur podmieniane są mapy krawędzi wchodzących i wychodzących
      // poprzez usunięcie (w stosunku do pierwotnych map) pozycji odpowiadających usuwanej krawędzi
      val e = edge(edgeDes).get
      val edge_id = e.ID
      val src_node_struct = findNodeStruct(e.SrcNode).get.asInstanceOf[NodeStruct[ND, ED]]
      val dst_node_struct = findNodeStruct(e.DstNode).get.asInstanceOf[NodeStruct[ND, ED]]

      val new_nodes = (src_node_struct :: dst_node_struct :: Nil).foldLeft(Nodes) {(n, ns) =>
        {n updated (n.indexOf(ns), ns.copy(InEdges = ns.InEdges - e.ID, OutEdges = ns.OutEdges - e.ID).asInstanceOf[NodeStruct[ND, ED]])}}

      newInstance(new_nodes)
  }
}
