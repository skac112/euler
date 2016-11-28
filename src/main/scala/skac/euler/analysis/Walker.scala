/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package skac.euler.analysis

import skac.euler.General._
import skac.euler._

class Walker[ND, ED](Graph: Graph[ND, ED]) {

  import skac.euler.General._
  import skac.euler._

  private var WalkList = List[NodeDesignator]()
  class WalkResult

//  object WalkContinue {
//    def WalkFirst(Edges: Set[ED]) = new WalkContinue(Edges, Set.empty)
//  }

  case class WalkContinue(ToTop: List[EdgeInfo[ED]], ToBottom: List[EdgeInfo[ED]]) extends WalkResult
  case class WalkStop() extends WalkResult
  type WalkFun = (NodeInfo[ND], Set[EdgeInfo[ED]]) => WalkResult

  def walk(From: NodeDesignator, WalkFun: WalkFun) {
    WalkList :+ From
    doWalking(WalkFun)
  }

  private def doWalking(WalkFun:  WalkFun) {
//    val node_des: NodeDesignator = WalkList.head
//    val node_info = Graph.node(node_des)
//    val edges = Graph.edgesOfNode(node_des)
//    val result = WalkFun(node_info get, edges.toSet[EdgeInfo[ED]])
//    result match {
//      case WalkContinue(toTop: Set[EdgeInfo[ED]], toBottom: Set[EdgeInfo[ED]]) => {
//          val filtered_top = (toTop map {_.oppositeNode(node_des)}) -- WalkList
//          // dodanie na początek listy
//          WalkList :::= filtered_top
//      }
//      case WalkStop() => WalkList = List.empty
//    }
  }
}
