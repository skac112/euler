package skac.euler.generators

import skac.euler._

/**
 * Generator grafów. Klasy implementujące tworzą grafy o określonej strukturze (w ramach metody
 * {@link generate()}. W konstruktorze przekazywany jest graf początkowy (StartGraph), który służy
 * jako baza do tworzenia grafu wynikowego. Graf wynikowy (zwracany przez {@link generate()} może,
 * ale nie musi być tożsamy grafowi startowemu (Zależy to od tego, czy graf startowy jest
 * mutowalny czy też nie).
 */
abstract class GraphGenerator[G <: Graph[G, ND, ED], ND, ED](implicit startGraph: G,
 nodeDataGen: G => ND,
 edgeDataGen: G => ED) {
//  type G = Graph[ND, ED]
  def generate(): G
}
