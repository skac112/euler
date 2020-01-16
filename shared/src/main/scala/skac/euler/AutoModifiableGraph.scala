package skac.euler
import cats.Id

object AutoModifiableGraph {
  implicit def gTomg[G[ND, ED] <: Graph[ND, ED], ND, ED](g: G[ND, ED])(implicit m: GraphModifier[G[ND, ED], ND, ED])= AutoModifiableGraph(g)(m)
  implicit def mgTog[G[ND, ED] <: Graph[ND, ED], ND, ED](mg: AutoModifiableGraph[G, ND, ED]) = mg.g
}

import AutoModifiableGraph._

case class AutoModifiableGraph[G[ND, ED] <: Graph[ND, ED], ND, ED](g: G[ND, ED])(implicit m: GraphModifier[G[ND, ED], ND, ED])
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