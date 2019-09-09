package skac.euler

import General._
//import cats.Monad
import cats._
import cats.implicits._
import scala.collection.generic._

object GraphView {
  implicit def NodeInfo2NodeDesignator(NodeInfo: NodeInfo[_]) = NodeIDDesignator(NodeInfo.ID)
  implicit def EdgeInfo2EdgeDesignator(EdgeInfo: EdgeInfo[_]) = EdgeIDDesignator(EdgeInfo.ID)
  val NEIGHBOR_SIDE_FORWARD = 0;
  val NEIGHBOR_SIDE_BACKWARD = 1;
  val NEIGHBOR_SIDE_BOTH = 2;
}

/**
 * Something like a graph but only with "local" information exposed and
 * "read-only". Agnostic about structure as a whole (maybe infinite) - collections of all nodes or edges
  * can't be obtained. Base trait for graph.
 */
trait GraphView[+ND, +ED, M[_]] {
  import General._
  import GraphView._
  implicit def m: Monad[M]
//  type G <: GraphView[ND, ED]
//  type ThisNodeInfo = NodeInfo[ND]
//  type ThisEdgeInfo = EdgeInfo[ED]
//  type Edges = Set[EdgeInfo[ED]]
//  type NodesT = Set[NodeDesignator]
//  type NodeInfos[SND >: ND] = Set[NodeInfo[SND]]
//  type EdgeInfos[SED >: ED] = Set[EdgeInfo[SED]]

  /**
   * Obiekt umożliwiający porównywanie (z wykorzystaniem konwersji implicit) dwóch desygnatorów
   * węzła. Porownanie takie na poziomie desygnatorów nie zawsze jest możliwe, bo przy
   * różnych typach desygnatorów potrzebne sa dane wezla, a wiec potrzebne jest odniesienie
   * do grafu.
   */
//  case class NodeDesignatorComp(nodeDes: NodeDesignator) {
//    def ===(other: NodeDesignatorComp): M[Boolean] = (this.nodeDes, other.nodeDes) match {
//      case (NodeIDDesignator(id1), NodeIDDesignator(id2)) => m.pure(id1 == id2)
//      case (NodeIdxDesignator(idx1), NodeIdxDesignator(idx2)) => m.pure(idx1 == idx2)
//      case (NodeDataDesignator(data1: Any), NodeDataDesignator(data2: Any)) => m.pure(data1 == data2)
//
//      case (des1, des2) => for {
//        n1 <- node(des1)
//        n2 <- node(des2)
//      } yield n1 == n2
//    }
//
//    override def equals(other: Any) = other match {
//      case nd: NodeDesignator => ===(NodeDesignatorComp(nd))
//      case ndc: NodeDesignatorComp => ===(ndc)
//      case _ => false
//    }
//  }

//  case class EdgeDesignatorComp(edgeDes: EdgeDesignator) {
//    def ===(other: EdgeDesignatorComp): Boolean = (this.edgeDes, other.edgeDes) match {
//      case (EdgeIDDesignator(id1), EdgeIDDesignator(id2)) => id1 == id2
//      case (EdgeIdxDesignator(idx1), EdgeIdxDesignator(idx2)) => idx1 == idx2
//      case (EdgeDataDesignator(data1: Any), EdgeDataDesignator(data2: Any)) => data1 == data2
//      case (EdgeNodesDesignator(src_node1, dst_node1), EdgeNodesDesignator(src_node2, dst_node2)) =>
//       NodeDesignatorComp(src_node1) === NodeDesignatorComp(src_node2) &&
//       NodeDesignatorComp(dst_node1) === NodeDesignatorComp(dst_node2)
//
//      case (des1: EdgeDesignator, des2: EdgeDesignator) => edge(des1) == edge(des2)
//    }
//
//    override def equals(Other: Any) = Other match {
//      case ed: EdgeDesignator => ===(EdgeDesignatorComp(ed))
//      case edc: EdgeDesignatorComp => ===(edc)
//      case _ => false
//    }
//  }

//  implicit def NodeDesignator2NodeDesignatorComp(NodeDes: NodeDesignator) = NodeDesignatorComp(NodeDes)
//  implicit def NodeDesignatorComp2NodeDesignator(NodeDesignatorComp: NodeDesignatorComp) = NodeDesignatorComp.nodeDes
//  implicit def EdgeDesignator2EdgeDesignatorComp(EdgeDes: EdgeDesignator) = EdgeDesignatorComp(EdgeDes)
  implicit def NodeInfo2NodeDesignator[SND >: ND](NodeInfo: NodeInfo[SND]) = NodeIDDesignator(NodeInfo.ID)
//  implicit def NodeInfo2NodeDesignatorComp[SND >: ND](NodeInfo: NodeInfo[SND]) = NodeDesignatorComp(NodeInfo.ID.id)
  implicit def EdgeInfo2EdgeDesignator[SED >: ED](EdgeInfo: EdgeInfo[SED]) = EdgeIDDesignator(EdgeInfo.ID)
//  implicit def EdgeInfo2EdgeDesignatorComp[SED >: ED](EdgeInfo: EdgeInfo[SED]) = EdgeDesignatorComp(EdgeInfo.ID.eid)

  def node[SND >: ND](nodeDes: NodeDesignator): M[Option[NodeInfo[SND]]]

  def edge[SED >: ED](edgeDes: EdgeDesignator): M[Option[EdgeInfo[SED]]]

//  def edges[SED >: ED](nodeDes: NodeDesignator): Set[EdgeInfo[SED]]

  /**
   * Określa, czy dana krawędź znajduje się pomiędzy danymi węzłami.
   */
  private def edgeIsBetween[SED >: ED](ei: EdgeInfo[SED], nd1: NodeDesignator,
   nd2: NodeDesignator, directed: Boolean): M[Boolean] = for {
    snd <- idDes(ei.SrcNode)
    dnd <- idDes(ei.DstNode)
    nd1 <- idDes(nd1)
    nd2 <- idDes(nd2)
  } yield (snd == nd1 && dnd == nd2 || !directed && snd == nd2 && dnd == nd1)

  /**
   * Określa, czy między podanymi węzłami znajduje się krawędź. W zależności od
   * parametru Directed szuka krawędzi w kierunku od węzła wskazanego przez
   * NodeDes1 do węzła wskazanego przez NodeDes2 albo krawędzi o dowolnym kierunku
   * pomiędzy tymi węzłami
   */
  def edgeExists(nd1: NodeDesignator, nd2: NodeDesignator, directed: Boolean): M[Boolean] = for {
    es <- if (directed) outEdges(nd1) else edges(nd1)
    es_b <- es.toList.existsM(edgeIsBetween(_, nd1, nd2, directed))
  } yield es_b


  /**
   * Returns edge between two nodes. If there are multiple edges (multigraph),
   * any of them can be returned.
   */
  def edgeBetween[SED >: ED](nd1: NodeDesignator, nd2: NodeDesignator, directed: Boolean): M[Option[EdgeInfo[SED]]] =
    for {
      es <- if (directed) outEdges(nd1) else edges(nd2)
    } yield (if (es.isEmpty) None else Some(es.toList.apply(0)))

  def edgesBetween[SED >: ED](nd1: NodeDesignator, nd2: NodeDesignator, directed: Boolean): M[Set[EdgeInfo[SED]]] =
    if (directed) {
      for {
        out_edges <- outEdges[SED](nd1)
        in_edges <- inEdges[SED](nd2)
      } yield out_edges & in_edges
    } else {
      for {
        out_edges_1 <- outEdges[SED](nd1)
        in_edges_2 <- inEdges[SED](nd2)
        out_edges_2 <- outEdges[SED](nd2)
        in_edges_1 <- inEdges[SED](nd1)
      } yield out_edges_1 & in_edges_2 | out_edges_2 & in_edges_1
    }

  /**
    * Set of edges of particular node. Parameter direction enables to select in
    * edges, out edges or both types of edges.
    * @param NodeDes
    * @param Direction
    * @return Set of edges of node.
    */
  def edges[SED >: ED](nd: NodeDesignator, direction: Int = NEIGHBOR_SIDE_BOTH): M[Set[EdgeInfo[SED]]]

  def inEdges[SED >: ED](nd: NodeDesignator): M[Set[EdgeInfo[SED]]] = edges[SED](nd, NEIGHBOR_SIDE_BACKWARD)

  def outEdges[SED >: ED](nd: NodeDesignator): M[Set[EdgeInfo[SED]]] = edges[SED](nd, NEIGHBOR_SIDE_FORWARD)

  def degree(nd: NodeDesignator): M[Int] = for {
    ed <- edges(nd)
  } yield ed.size

  def inDegree(nd: NodeDesignator): M[Int] = for {
    in_edges <- inEdges(nd)
  } yield in_edges.size

  def outDegree(nd: NodeDesignator): M[Int] = for {
    out_edges <- outEdges(nd)
  } yield out_edges.size

  def neighbors[SND >: ND](nd: NodeDesignator, Direction: Int): M[Set[NodeInfo[SND]]] = for {
    all_edges <- edges(nd, Direction)
    loops <- all_edges.toList.traverse(edgeIsLoop(_))
    nodes <- ((all_edges.toList zip loops) filterNot (_._2)).traverse {e => for {
      op_node_des <- oppositeNode(nd, e._1)
      op_node <- node(op_node_des)
    } yield op_node }
  } yield (nodes map { _.get } toSet)

  def neighbors[SND >: ND](nodes: Set[NodeDesignator], direction: Int): M[Set[NodeInfo[SND]]] = ???

  /**
   * Zwraca zbiór węzłów oddalonych o LayerIdx od węzła określonego przez NodeDes. dla LayerIdx = 0
   * zwraca węzeł NodeDes, dla LayerIdx = 1 zbiór sąsiadów węzła NodeDes itd., dla LayerIdx zbiór
   * sąsiadów sąsiadów (z wyłączeniem sąsiadów i węzła danego) itd.
   */
  def neighborLayer[SND >: ND](nodeDes: NodeDesignator, layerIdx: Int, direction: Int): M[Set[NodeInfo[SND]]] =
    neighborLayer(Set(nodeDes), layerIdx, direction)

  /**
   * Wyznacza n-ta "warstwe" zbioru wezlow, czyli wezly oddalone o n krawedzi od dowolnego wezla
   * poczatkowego zbioru. Dozwolone przejscia po krawedziach (w przod, w tyl lub w obu kierunkach)
   * okreslone sa przez argument Direction
   */
  def neighborLayer[SND >: ND](nodes: Set[NodeDesignator], layerIdx: Int, direction: Int): M[Set[NodeInfo[SND]]] = {
    val start_nodes_o_m: M[List[Option[NodeIDDesignator]]] = nodes.toList.map(nd => idDes(nd)).sequence

    val start_nodes_l: M[List[NodeIDDesignator]] = for {
      start_nodes_o <- start_nodes_o_m
      // filtering out None's
      start_nodes = for {
        id_des_o <- start_nodes_o
        id_des <- id_des_o
      } yield id_des
    } yield start_nodes

    val start_nodes: M[Set[NodeIDDesignator]] = start_nodes_l map {_.toSet}

//    val list_m: M[List[Set[NodeIDDesignator]]] = List(start_nodes, start_nodes).sequence
    val start_m: M[(Set[NodeIDDesignator], Set[NodeIDDesignator])] = start_nodes.map { nodes: Set[NodeIDDesignator] =>
      (nodes, nodes) }

    val all_curr_m: M[(Set[NodeIDDesignator], Set[NodeIDDesignator])] = (0 until layerIdx).foldLeft(start_m) {
      case (all_curr_m, _) => for {
        all_curr <- all_curr_m
        all = all_curr._1
        curr = all_curr._2.toSet[NodeDesignator]
        new_layer <- neighbors(curr, direction) map {neighbors => neighbors map {_.ID.id}}
      } yield ((all ++ new_layer, new_layer -- all))
    }

    val curr_m: M[Set[NodeIDDesignator]] = all_curr_m map {_._2}

    for {
      curr <- curr_m

      nodes_o_m = for {
        nd <- curr
      } yield node(nd)

      nodes_o <- nodes_o_m.toList.sequence

      // filtering out None's
      nodes = for {
        node_o <- nodes_o
        node <- node_o
      } yield node

    } yield nodes.toSet


//    var all_layers = smartNodeColl(nodes)
//    var res = smartNodeColl(nodes)
//
//    (0 until layerIdx) foreach ((x: Int) => {
//      res  = (res flatMap {ndc: NodeDesignatorComp => { neighbors(ndc.nodeDes, direction) } map { ni: NodeInfo[ND] =>
//         NodeDesignatorComp(ni.ID.id) }
//      }).toSet -- all_layers
//
//    	all_layers = all_layers ++ res
//    })
//
//    res map (node(_).get) toSet
  }

  def edgeLayer[SED >: ED](nodes: Set[NodeDesignator], layerIdx: Int, direction: Int): M[Set[EdgeInfo[SED]]] = for {
    // we use the fact that n-th layer of edges is the set of edges incident (with appropriate direction) with
    // (n-1)-th layer of neighbor nodes
    neighbors <- neighborLayer(nodes, layerIdx - 1, direction)
    edges <- neighbors.toList.flatTraverse { edges(_, direction) map {_.toList} }
  } yield edges toSet

//    /**
//   * Wyznacza n-ta warstwe krawedzi w stosunku do wybranego zbioru wezlow. N-ta warstwa krawedzi
//   * to warstwa laczaca wezly z warstwy n-1 z wezlami warstyw n z zachowaniem kierunkow dozwolonych
//   * przejsc okreslonych przez argument Direction. Tak wiec numerowanie warstw krawedzi zaczyna sie
//   * od 1 i 1-sza warstwa oznacza krawedzie pomiedzy wezlami Nodes a 1-sza warstwa wezlow dla Nodes.
//   * Przyjecie numeracji warstw krawedzi od 1 sprawia, ze fragment grafu okreslony przez wezly i
//   * krawedzie od 0 do okreslonej warstwy sa podgrafem grafu oryginalnego
//   */
//  def edgeLayer[SED >: ED](nodes: Set[NodeDesignator], LayerIdx: Int, Direction: Int): Set[EdgeInfo[SED]] = {
//    val start_nodes_o_m: M[List[Option[NodeIDDesignator]]] = nodes.toList.map(nd => idDes(nd)).sequence
//
//    val start_nodes_l: M[List[NodeIDDesignator]] = for {
//      start_nodes_o <- start_nodes_o_m
//      // filtering out None's
//      start_nodes = for {
//        id_des_o <- start_nodes_o
//        id_des <- id_des_o
//      } yield id_des
//    } yield start_nodes
//
//    val start_nodes: M[Set[NodeIDDesignator]] = start_nodes_l map {_.toSet}
//
//    val list_m: M[List[Set[NodeIDDesignator]]] = List(start_nodes, start_nodes).sequence
//    val start_m: M[(Set[NodeIDDesignator], Set[NodeIDDesignator])] = list_m.map {list => (list(0), list(1))}
//
//    val all_curr_m: M[(Set[NodeIDDesignator], Set[NodeIDDesignator])] = (0 until layerIdx).foldLeft(start_m) {
//      case (all_curr_m, _) => for {
//        all_curr <- all_curr_m
//        all = all_curr._1
//        curr = all_curr._2.toSet[NodeDesignator]
//        new_layer <- neighbors(curr, direction) map {neighbors => neighbors map {_.ID.id}}
//      } yield ((all ++ new_layer, new_layer -- all))
//    }
//
//    val curr_m: M[Set[NodeIDDesignator]] = all_curr_m map {_._2}
//
//    for {
//      curr <- curr_m
//
//      nodes_o_m = for {
//        nd <- curr
//      } yield node(nd)
//
//      nodes_o <- nodes_o_m.toList.sequence
//
//      // filtering out None's
//      nodes = for {
//        node_o <- nodes_o
//        node <- node_o
//      } yield node
//
//    } yield nodes.toSet
//
//
//
//
//
//    val all_node_layers: M[List[Option[NodeIDDesignator]]] = nodes.toList.map(nd => idDes(nd)).sequence
//    var node_layer = smartNodeColl(Nodes)
//    var prev_layer = node_layer
//
//    (1 to LayerIdx) foreach ((x: Int) => {
//        node_layer  = (node_layer flatMap {ndc: NodeDesignatorComp => {neighbors(ndc.nodeDes, Direction)} map { ni: NodeInfo[ND] =>
//           NodeDesignatorComp(ni.ID.id)}
//        }).toSet -- all_node_layers
//
//    	all_node_layers = all_node_layers ++ node_layer
//
//        if (x == LayerIdx - 1)
//          prev_layer = node_layer
//    })
//
//    // prev_layer oznacza wezly warstwy LayerIdx - 1
//    // node_layer oznacza wezly warstwy LayerIdx
//    // warstwa krawedzi nr LayerIdx to krawedzie pomiedzy tymi dwiema warstwami wezlow
//    // krawedzie te sa wyznaczane jako czesc wspolna zbioru krawedzi odpowiednio skierowanych incydentnych z
//    // wezlami warstwy LayerIdx - 1 oraz zbioru krawedzi odpowiednio (przeciwnie) skierowanych incydentnych z
//    // wezlami warstwy LayerIdx
//    ((prev_layer flatMap {ndc: NodeDesignatorComp => edges(ndc, Direction)}) toSet) intersect
//     ((node_layer flatMap {ndc: NodeDesignatorComp => edges(ndc, oppositeDirection(Direction))}) toSet)
//    //edge_layer map (edge(_).get) toSet
//  }

  def edgeLayer[SED >: ED](node: NodeDesignator, layerIdx: Int, Direction: Int): M[Set[EdgeInfo[SED]]] =
    edgeLayer[SED](Set(node), layerIdx, Direction)

  def oppositeDirection(Direction: Int) = Direction match {
    case NEIGHBOR_SIDE_FORWARD => NEIGHBOR_SIDE_BACKWARD
    case NEIGHBOR_SIDE_BACKWARD => NEIGHBOR_SIDE_FORWARD
    case NEIGHBOR_SIDE_BOTH => NEIGHBOR_SIDE_BOTH
  }

//  def nodeID(NodeDes: NodeDesignator): Option[Any] = NodeDes match {
//    case NodeIDDesignator(id) => Some(id)
//    case _ => node(NodeDes) map {_.ID}
//  }

  def idDes(nd: NodeDesignator): M[Option[NodeIDDesignator]] = nd match {
    case nd: NodeIDDesignator => impPure(Some(nd))
    case _ => node(nd) map {ni_o => ni_o.map {_.ID.id}}
  }

  def idDes(ed: EdgeDesignator): M[Option[EdgeIDDesignator]] = ed match {
    case ed: EdgeIDDesignator => impPure(Some(ed))
    case _ => edge(ed) map {ei_o => ei_o.map {_.ID.eid}}
  }

  def edgeID(ed: EdgeDesignator): M[Option[Any]] = ed match {
    case ed: EdgeIDDesignator => impPure(Some(ed))
    case _ => edge(ed) map { ei_o => ei_o.map { _.ID } }
  }

  /**
   * Przekształca kolekcję desygnatorów węzła na kolekcję obiektów {@link NodeDesignatorComp},
   * co umożliwia "sprytne" porównywanie desygnatorów (desygnatory różnych typów mogą odnosić się
   * do tego samego węzła).
   */
//  def smartNodeColl[B, That](nodes: Traversable[NodeDesignator])
//   (implicit bf: CanBuildFrom[Traversable[NodeDesignator], B, That]): M[Traversable[NodeDesignator]] =
//    nodes map { n_des => NodeDesignatorComp(n_des) } sequence

  def oppositeNode(nodeDes: NodeDesignator, Edge: EdgeDesignator): M[NodeDesignator] = for {
    nid <- idDes(nodeDes)
    e_info <- edge(Edge)
    src_node <- idDes(e_info.get.SrcNode)
  } yield (if (src_node == nid) e_info.get.DstNode else e_info.get.SrcNode)

  def edgeIsLoop(edgeDes: EdgeDesignator): M[Boolean] = for {
    ei_o <- edge(edgeDes)
    src_idd <- idDes(ei_o.get.SrcNode)
    dst_idd <- idDes(ei_o.get.DstNode)
  } yield src_idd == dst_idd


  /**
   * Returns pair of nodes incident to an edge.
   */
  def incident[SND >: ND](edgeDes: EdgeDesignator): M[(NodeInfo[SND], NodeInfo[SND])] = for {
    e <- edge(edgeDes) map { _.get }
    src_node <- node(e.SrcNode) map { _.get }
    dst_node <- node(e.DstNode) map { _.get }
  } yield (src_node, dst_node)

  /**
    * Determines if an edge is incident with a node. Result is wrapped in a graph view monad.
    * @param ei
    * @param nd
    * @tparam SED
    * @return
    */
  def isEdgeOf(ei: EdgeInfo[_], nd: NodeDesignator): M[Boolean] = for {
    // source node id designator
    nid_src <- idDes(ei.SrcNode) map { _.get }
    // dst node id designator
    nid_dst <- idDes(ei.DstNode) map { _.get }
    // id designator of node given
    nid <- idDes(nd) map { _.get }
  } yield nid_src == nid || nid_dst == nid

  /**
    * Determines if an edge is incident with a node for given direction. Result is wrapped in a graph view monad.
    * @param ei
    * @param nd
    * @param direction
    * @tparam SED
    * @return
    */
  def isEdgeOf(ei: EdgeInfo[_], nd: NodeDesignator, direction: Int) = for {
    // id designator of node given
    nid <- idDes(nd) map { _.get }
    res <- direction match {
      case NEIGHBOR_SIDE_FORWARD => for {
        nid_src <- idDes(ei.SrcNode) map { _.get }
      } yield nid_src == nid
      case NEIGHBOR_SIDE_BACKWARD => for {
        nid_dst <- idDes(ei.DstNode) map { _.get }
      } yield nid_dst == nid
      case NEIGHBOR_SIDE_BOTH => for {
        nid_src <- idDes(ei.SrcNode) map { _.get }
        nid_dst <- idDes(ei.DstNode) map { _.get }
      } yield nid_src == nid || nid_dst == nid
    }
  } yield res

  def isInEdgeOfNode(ei: EdgeInfo[_], nd: NodeDesignator) = for {
    // id designator of node given
    nid <- idDes(nd) map { _.get }
    nid_dst <- idDes(ei.DstNode) map { _.get }
  }  yield nid_dst == nid


  protected def isOutEdgeOfNode(ei: EdgeInfo[_], nd: NodeDesignator) = for {
    // id designator of node given
    nid <- idDes(nd) map { _.get }
    nid_dst <- idDes(ei.SrcNode) map { _.get }
  } yield nid_dst == nid

  /**
   * Returns set of edges adjacent to a given edge.
   */
  def adjacent[SED >: ED](edgeDes: EdgeDesignator): Set[EdgeInfo[SED]] = ???

  def impPure[T](something: T): M[T] = implicitly[Monad[M]].pure(something)
}
