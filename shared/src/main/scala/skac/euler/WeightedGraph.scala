package skac.euler
import General._

trait WeightedGraph[ND, ED] extends Graph[ND, ED] {
  import General._

  def getNodeWeight(NodeDes: NodeDesignator): Double
  def getEdgeWeight(EdgeDes: EdgeDesignator): Double

  /**
   * Ustawia wagę węzła. Metoda przystosowana jest do implementacji immutowalnych, bo może zwracać
   * nowy graf, różniący się od wyjściowego tylko odpowiednią wartością wagi w odpowiednim węźle
   */
  def setNodeWeight(NodeDes: NodeDesignator, Weight: Double): WeightedGraph[ND, ED]

  /**
   * Ustawia wagę krawędzi. Metoda przystosowana jest do implementacji immutowalnych, bo może zwracać
   * nowy graf, różniący się od wyjściowego tylko odpowiednią wartością wagi w odpowiedniej krawędzi.
   */
  def setEdgeWeight(EdgeDes: EdgeDesignator, Weight: Double): WeightedGraph[ND, ED]
}
