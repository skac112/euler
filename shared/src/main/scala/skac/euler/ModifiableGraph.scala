package skac.euler

trait ModifiableGraph[G <: ModifiableGraph[G, ND, ED], ND, ED] extends Graph[ND, ED] { self: G =>
  def addNode(data: ND): G
  def addEdge(data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator): G
  def removeNode(nodeDes: NodeDesignator): G
  def removeEdge(edgeDes: EdgeDesignator): G
  def removeNode(g: G, nodeData: ND): G = removeNode(NodeDataDesignator(nodeData))
  def removeEdge(g: G, edgeData: ED): G = removeEdge(EdgeDataDesignator(edgeData))
  def clear = (1 to nodeCount).foldLeft[G](this) { (_, _) => removeNode(0.i) }
  def +(Data: ND) = addNode(Data)
  def +->(data: ED, srcNode: NodeDesignator, dstNode: NodeDesignator) = addEdge(data, srcNode, dstNode)
  def ++(nodes: ND*): G = nodes.foldLeft(this) {(graph, node_data) => graph + node_data }
  def -(nodeDes: NodeDesignator): G = removeNode(nodeDes)
  def -->(edgeDes: EdgeDesignator): G = removeEdge(edgeDes)

}
