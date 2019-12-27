/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package skac.euler

import skac.euler.impl._
import skac.euler.General._

object Main extends App {
    (1000 to 10000 by 1000) foreach {i => {
      var t = System.currentTimeMillis
      var g = skac.euler.impl.fastindex.immutable.Graph[Any, Any]()
      var g2 = (1 to i).foldLeft(g) {(g, i) => g.addNode(i)}
      System.out.println(System.currentTimeMillis - t)
    }}

//    (1000 to 10000 by 1000) foreach {i => {
//      var t = System.currentTimeMillis
//      var g: Graph[Any, Any] = new skac.euler.impl.fastindex.mutable.Graph[Any, Any]
//      var g2 = (1 to i).foldLeft(g) {(g, i) => g.addNode(i)}
//      System.out.println(System.currentTimeMillis - t)
//    }}
}
