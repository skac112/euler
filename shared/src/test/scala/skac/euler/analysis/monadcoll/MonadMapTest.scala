package skac.euler.analysis.monadcoll

import cats.Id
import utest._

object MonadMapTest extends TestSuite {

  case class MonadicMapImpl(map: Map[Int, Int] = Map.empty) extends MonadMap[Int, Int, Id, MonadicMapImpl] {
    override def baseMap: Map[Int, Int] = map

    override def canHaveKey(k: Int): Boolean = true

    override def compValueM(k: Int): Id[Any] = k % 2

    override def newInstance(map: Map[Int, Int]): MonadicMapImpl = MonadicMapImpl(map)
  }

  val mm = MonadicMapImpl()

  override def tests = Tests {
    'Adding_new_elements - {
      val m = mm + (1 -> 1) + (2 -> 2)
      assert(m == MonadicMapImpl(Map((1 -> 1), (2 -> 2))))
    }

    'Updating_existing_key - {
      val m = mm + (1 -> 1) + (2 -> 2) + (1 -> 10)
      assert(m == MonadicMapImpl(Map((1 -> 10), (2 -> 2))))
    }

    'Updating_key_existing_due_to_comparator_value - {
      val m = mm + (1 -> 1) + (2 -> 2) + (3 -> 10)
      assert(m == MonadicMapImpl(Map((1 -> 10), (2 -> 2))))
    }
  }
}
