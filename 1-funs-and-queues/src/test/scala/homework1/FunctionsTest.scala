package homework1

import org.scalatest.{FlatSpec, Matchers}
import Functions._

class FunctionsTest extends FlatSpec with Matchers {
  "fromDigits" should "form a decimal number" in {
    fromDigits(List(1, 2, 3)) shouldBe 123
  }

  it should "form a hex number" in {
    fromDigits(List(1, 12, 4), 16) shouldBe 452
  }

  "parseInteger" should "parse a decimal number" in {
    parseInteger("123") shouldBe 123
  }

  it should "parse a hex number" in {
    parseInteger("1C4", 16) shouldBe 452
  }

  "zipMap" should "transform two lists" in {
    zipMap(List(1, 2, 3), List(4, 5, 6), _ * _) shouldBe List(4, 10, 18)
  }

  "countCoinChangeVariants" should "count the ways to give a change" in {
    countCoinChangeVariants(List(1, 2, 5), 6) shouldBe 5
    countCoinChangeVariants(List(1), 6) shouldBe 1
    countCoinChangeVariants(List(1, 2, 2, 5), 6) shouldBe 5
  }

  "bfsTraversal" should "give the traversed path" in {
    val graph = (i: Int) => i match {
      case 1 => List(2, 5, 8)
      case 2 => List(1, 3, 6)
      case 3 => List(2, 4)
      case 4 => List(3)
      case 5 => List(6)
      case 6 => List(7)
      case 8 => List(9)
      case _ => Nil
    }

    bfsTraversal(1, 6, graph).toSeq shouldBe Seq(1, 2, 5, 8, 3, 6)
    bfsTraversal(4, 6, graph).toSeq shouldBe Seq(4, 3, 2, 1, 6)
  }
}
