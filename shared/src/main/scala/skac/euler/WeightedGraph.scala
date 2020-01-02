package skac.euler
import cats.Id

trait WeightedGraph[ND, ED] extends Graph[ND, ED] with WeightedGraphView[ND, ED, Id] {

  /**
    * Sets new node weight.
    */
  def setNodeWeight(NodeDes: NodeDesignator, Weight: Double): WeightedGraph[ND, ED]

  /**
    * Ustawia wagę krawędzi. Metoda przystosowana jest do implementacji immutowalnych, bo może zwracać
    * nowy graf, różniący się od wyjściowego tylko odpowiednią wartością wagi w odpowiedniej krawędzi.
    */
  def setEdgeWeight(EdgeDes: EdgeDesignator, Weight: Double): WeightedGraph[ND, ED]
}