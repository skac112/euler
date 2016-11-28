package skac.euler.fungraphs

import skac.euler._
import skac.euler.General._
import skac.euler.impl.fastindex._
import scala.annotation.tailrec

object FunGraph {
  case class InputInfo[T, ED](nodeInfo: NodeInfo[GraphFun[T, ED]], inputs: Map[EdgeInfo[ED], T])
  type GraphFun[T, ED] = (InputInfo[T, ED] => T)
  type Adapt2Fun[T, ED] = InputInfo[T, ED] => Tuple2[T, T]
  type Adapt3Fun[T, ED] = InputInfo[T, ED] => Tuple3[T, T, T]
  type Adapt4Fun[T, ED] = InputInfo[T, ED] => Tuple4[T, T, T, T]
  type AdaptFun[T, ED] = InputInfo[T, ED] => List[T]
}

import FunGraph._

/**
 * Graf funkcji.
 */
abstract class FunGraph[T, ED] extends mutable.Graph[GraphFun[T, ED], ED] {

  def Fun2Adapter(AdaptFun: Adapt2Fun[T, ED], Fun: (T, T) => T): GraphFun[T, ED] =
    (Input: InputInfo[T, ED]) => {
      val adapt_tuple = AdaptFun(Input)
      Fun(adapt_tuple._1, adapt_tuple._2)
    }

  def Fun3Adapter(AdaptFun: Adapt3Fun[T, ED], Fun: (T, T, T) => T): GraphFun[T, ED] =
    (Input: InputInfo[T, ED]) => {
      val adapt_tuple = AdaptFun(Input)
      Fun(adapt_tuple._1, adapt_tuple._2, adapt_tuple._3)
    }

  def Fun4Adapter(AdaptFun: Adapt4Fun[T, ED], Fun: (T, T, T, T) => T): GraphFun[T, ED] =
    (Input: InputInfo[T, ED]) => {
      val adapt_tuple = AdaptFun(Input)
      Fun(adapt_tuple._1, adapt_tuple._2, adapt_tuple._3, adapt_tuple._4)
    }

  def FunListAdapter(AdaptFun: AdaptFun[T, ED], Fun: (T*) => T): GraphFun[T, ED] =
    (Input: InputInfo[T, ED]) => Fun(AdaptFun(Input): _*)

  /**
   * Adapter funkcji, stosowany w przypadku, gdy wszystkie argumenty funkcji pelnia
   * jednakowa role (np. funkcja sumy). W tym przypadku adapter laczy wyjscia z parametrami wywolania
   * funkcji w sposob nieokreslony.
   */
  //def FunRegAdapter(Fun: (T*) => T): GraphFun[T, ED] =

  var Outcome: Option[T]

  class FunGraphNodeInfo(override val ID: Any, override val Data: (Traversable[T]) => T,
   val Processed: Boolean = false) extends NodeInfo(ID, Data)


  def process: T = {
    markUnprocessed
    processLoop
  }

  //def process(IterCount: Int): T = {}

  @tailrec
  private def processLoop: T = Outcome match {
      case Some(outcome) => outcome
      case _ => {pickUnprocessed; processNode; processLoop}
  }

//  @tailrec
//  def processLoop: T = pickUnprocessed match {
//      case Some(n) => {processNode; processLoop}
//      case _ => Outcome
//  }

  // TODO:
  def markUnprocessed

  // TODO:
  def pickUnprocessed: Option[NodeDesignator]


  def processNode = {

  }

  def OutcomeNode: NodeDesignator
  def OutcomeNode_=(NodeDes: NodeDesignator)
  def StartValues: Map[NodeDesignator, T]
  def StartValues_=(Values: Map[NodeDesignator, T])

  def newNodeInfo(Fun: (Traversable[T]) => T) = new FunGraphNodeInfo(newNodeID, Fun)

//  override def newEdgeInfo(Data: ED, SrcNode: NodeDesignator, DstNode: NodeDesignator) =
//    new CollapsableEdgeInfo(newEdgeID, Data, SrcNode, DstNode, 1.0, "", "", Set.empty[EdgeDesignator])

}
