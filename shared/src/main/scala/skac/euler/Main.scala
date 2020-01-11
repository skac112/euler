/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package skac.euler

import skac.euler.impl._
import skac.euler.AutoModifiableGraph._
import skac.euler.impl.fastindex.immutable.Graph._

object Main extends App {
    class MyGraph extends skac.euler.impl.fastindex.immutable.Graph[Int, String] {
      def nazwa = "gn"
    }

    type GraphType = skac.euler.impl.fastindex.immutable.Graph[Int, String];
    (1000 to 10000 by 1000) foreach {i => {
      var t = System.currentTimeMillis
      val g = skac.euler.impl.fastindex.immutable.Graph[Int, String]()

//      val gn = new MyGraph()

//      val m = skac.euler.impl.fastindex.immutable.AbstractGraph.modifier[G, Int, String]
//      val mg: ModifiableGraph[_, Int, String] = AutoModifiableGraph.gTomg[GraphType, Int, String](g)
      val g2 = (1 to i).foldLeft(g) {(g, i) => g.addNode(i)}
//      val g2 = (1 to i).foldLeft(gn) {(g, i) => mgTog[MyGraph, Int, String](g.addNode(i))}
      val gm1 = modify(g2, 10)
      val gm2 = modify2(g2, 20)
      val gm3 = modify3(g2, 10)
      println(s"Node count: ${g2.nodeCount}")
      println(s"gm1's node count: ${gm1.nodeCount}")
      println(s"gm2's node count: ${gm2.nodeCount}")
      println(s"gm3's node count: ${gm3.nodeCount}")
      System.out.println(System.currentTimeMillis - t)
    }}

    def modify[G[ND, ED] <: Graph[ND, ED], ND, ED](g: G[ND, ED], data: ND)(implicit m: GraphModifier[G[ND, ED], ND, ED]): G[ND, ED] = {
      m.addNode(g, data)
    }

    def modify2[ND, ED, G[ND, ED] <: Graph[ND, ED]](mg: AutoModifiableGraph[G, ND, ED], data: ND): G[ND, ED] = {
      mg.addNode(data)
    }

    def modify3[ND, ED, G <: ModifiableGraph[G, ND, ED]](mg: ModifiableGraph[G, ND, ED], data: ND): G = {
      mg.addNode(data)
    }

//    (1000 to 10000 by 1000) foreach {i => {
//      var t = System.currentTimeMillis
//      var g: Graph[Any, Any] = new skac.euler.impl.fastindex.mutable.Graph[Any, Any]
//      var g2 = (1 to i).foldLeft(g) {(g, i) => g.addNode(i)}
//      System.out.println(System.currentTimeMillis - t)
//    }}
}
