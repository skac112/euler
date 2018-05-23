package skac.euler.varia

import skac.euler._
import skac.euler.General._

class RectLayout[ND](val Graph: WeightedGraph[AnyRef, _], val MatrixWidth: Int, val MatrixHeight: Int) {

//  import skac.graphs.General._
//  import Graph._
//
//  /**
//   * określa przyporządkowanie prostokąta do węzła + informacja o tym, czy pozycja jest ustalona
//   * (jeśli nie, to X i Y prostokąta mogą się zmienić - ściśle biorąc może nastąpić przyporządkowanie
//   * nowego prostokąta o innych X i Y i takich samych XSize i YSize)
//   */
//  type NodeAssignment = Map[NodeDesignator, Tuple2[RectArea, Boolean]]
//
//  private var Assignment = Map[NodeDesignator, NodeAssignment]()
//
//  case class RectArea(X: Int, Y: Int, XSize: Int, YSize: Int) {
//    def overlapsWith(Area: RectArea) = {
//      ((X >= Area.X) && (X < Area.X + Area.XSize) || (Area.X >= X) && (Area.X < X + XSize)) &&
//        ((Y >= Area.Y) && (Y < Area.Y + Area.YSize) || (Area.Y >= Y) && (Area.Y < Y + YSize))
//    }
//
//    /**
//     * Określa, czy dany obszar prostokątny "dotyka" obszaru reprezentowanego
//     * przez ten obiekt. Dotykanie dwóch obszarów rozumiane jest jako "stykanie"
//     * się krawędzi lub narożników ale z wykluczeniem zachodzenia na siebie
//     * (overlapping-u).
//     */
//    def touchesWith(Area: RectArea) = {
//      (((X + XSize == Area.X) || (Area.X + Area.XSize == X)) &&
//      ((X >= Area.X) && (X <= Area.X + Area.XSize) || (Area.X >= X) && (Area.X <= X + XSize))) ||
//      (((Y + YSize == Area.Y) || (Area.Y + Area.YSize == Y)) &&
//      ((X >= Area.X) && (X <= Area.X + Area.XSize) || (Area.X >= X) && (Area.X <= X + XSize)))
//    }
//  }
//
//
//
//  def doLayout(): Set[RectArea] {
////    assignRects()
////    while (!endConditions) {
////      val ns = nextLargestNode()
////      if isPlaced(ns)
////        place(ns)
////      placeNeighbours(ns)
////    }
//  }
//
//  private def assignRects() {
//    if (MatrixWidth * MatrixHeight < Graph.nodeCount)
//      throw new RuntimeException("Zbyt mały rozmiar matrycy");
//    // ilość "pola" przypadająca na jednostkę wagi
//    val weight_fct = MatrixWidth * MatrixHeight / (Graph.nodes map {n => Graph.getNodeWeight(n)} sum)
//    // posortowanie węzłów według rosnących wag
//    val node_sort = Graph.nodes.toSeq.sortWith {(n1, n2) => (Graph.getNodeWeight(n1) < Graph.getNodeWeight(n2))}
//    var matrix_area = MatrixWidth * MatrixHeight
//    Assignment = node_sort map {n => (n, (randomRectArea(Graph.getNodeWeight(n) * weight_fact))}
//
//
//
//  }
}
