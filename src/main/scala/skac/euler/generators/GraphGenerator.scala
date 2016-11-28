package skac.euler.generators

import skac.euler._

/**
 * Generator grafów. Klasy implementujące tworzą grafy o określonej strukturze (w ramach metody
 * {@link generate()}. W konstruktorze przekazywany jest graf początkowy (StartGrapgh), który służy
 * jako baza do tworzenia grafu wynikowego. Graf wynikowy (zwracany przez {@link generate()} może,
 * ale nie musi być tożsamy grafowi startowemu (Zależy to od tego, czy graf startowy jest
 * mutowalny czy też nie).
 */
abstract class GraphGenerator[ND, ED](startGraph: Graph[ND, ED], nodeDataGen: () => ND, edgeDataGen: () => ED) {

  def generate: Graph[ND, ED]
}
