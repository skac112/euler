/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package skac.euler.visualization

import skac.euler._
import java.awt.geom.Point2D
import java.awt.geom.Rectangle2D
import skac.euler.Graph

trait Graph2D[ND, ED] extends Graph[ND, ED] {
  import skac.euler.General._

  def nodeLocation(NodeDes: NodeDesignator): Point2D.Double

  def setNodeLocation(NodeDes: NodeDesignator, Location: Point2D.Double)

  private def addEdgeRect(Rect: Rectangle2D, Edge: EdgeInfo[ED]) = {
    Rect.add(edgeRect(Edge))
  }
  //private val FunAddEdgeRect: Function2[Rectangle2D, EdgeInfo, Unit] = ((Rect: Rectangle2D, Edge: EdgeInfo) => (Rect.add(edgeRect(Edge))))

  def range: Option[Rectangle2D] = nodeCount match {
    case 0 => None
    case _ => {
      val res: Rectangle2D = nodeCount match  {
        case 1 => {
          val loc = nodeLocation(NodeIdxDesignator(0));
          new Rectangle2D.Double(loc.getX(), loc.getY(), 0.0, 0.0)
        }
        case _ => {
          val loc = nodeLocation(NodeIdxDesignator(0))
            val res = new Rectangle2D.Double(loc.getX(), loc.getY(), 0.0, 0.0)
            for (i <- 1 until nodeCount)
              res.add(nodeLocation(NodeIdxDesignator(i)))
            res
        }
      }
      edges foreach {addEdgeRect(res, _)}
      //edges foreach (FunAddEdgeRect(res, _))
      Some(res)
    }
  }

  def edgeRect(Edge: EdgeInfo[ED]): Rectangle2D = {
    val p1 = nodeLocation(Edge.SrcNode)
    val p2 = nodeLocation(Edge.DstNode)
    val (min_x, max_x) = if (p1.getX < p2.getX) (p1.getX, p2.getX) else (p2.getX, p1.getX)
    val (min_y, max_y) = if (p1.getY < p2.getY) (p1.getY, p2.getY) else (p2.getY, p1.getY)
    new Rectangle2D.Double(min_x, min_y, max_x - min_x, max_y - min_y)
  }
}
