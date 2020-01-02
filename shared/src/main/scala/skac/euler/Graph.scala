package skac.euler

import cats.{Id, Monad}

import scala.collection.generic._

trait Graph[+ND, +ED] extends GraphView[ND, ED, Id] {
//  type NodeT = ND
//  type EdgeT = ED
  override def m = Monad[Id]
  import GraphView._

  def nodeCount: Int
  def edgeCount: Int

  /**
   * Returns set of nodes containing specific data.
   */
  def nodesOf[SND >: ND](data: SND): Set[NodeInfo[SND]] = nodes filter {_.Data == data} toSet

  /**
   * Returnes set of edges containing specific data.
   */
  def edgesOf[SED >: ED](data: SED): Set[EdgeInfo[SED]] = edges filter {_.Data == data} toSet

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

  override def equals(other: Any) = other match {
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
   * Displays some info about graph.
   */
  def about() {
    println(s"Basic graph characteristics: \r\nNode count: ${nodeCount}, edge count: ${edgeCount}.")
  }


}
