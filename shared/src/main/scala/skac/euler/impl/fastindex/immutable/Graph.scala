package skac.euler.impl.fastindex.immutable


case class Graph[ND, ED](pNodes: Vector[NodeStruct[ND, ED]] = Vector[NodeStruct[ND, ED]](),
                         newElemIdSeed: Option[Int] = None)
  extends AbstractGraph[Graph[ND, ED], ND, ED](pNodes, newElemIdSeed) {
  override def newInstance(nodes: Vector[NodeStruct[ND, ED]]) = new Graph[ND, ED](nodes, newNewElemIdSeed)
}
