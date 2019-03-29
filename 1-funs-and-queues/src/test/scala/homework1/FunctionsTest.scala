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

  it should "form a binary number" in {
    fromDigits(List(1, 0, 1, 1), 2) shouldBe 11
  }

  it should "be able to form a number with radix 35" in {
    fromDigits(List(3, 35), 35) shouldBe 140
  }

  "parseInteger" should "parse a decimal number" in {
    parseInteger("123") shouldBe 123
  }

  it should "parse a hex number" in {
    parseInteger("1C4", 16) shouldBe 452
  }

  it should "parse a binary number" in {
    parseInteger("1001", 2) shouldBe 9
  }

  it should "not be affected by leading zeroes" in {
    parseInteger("0000123") shouldBe 123
  }

  "zipMap" should "transform two lists" in {
    zipMap(List(1, 2, 3), List(4, 5, 6), _ * _) shouldBe List(4, 10, 18)
  }

  it should "be able to work with different length lists" in {
    zipMap(List(3, 6), List(20, 30, 40), (x, y) => y - x) shouldBe List(17, 24)
    zipMap(List(1, 2, 3, 4), List(1, 3), _ + _) shouldBe List(2, 5)
  }

  "countCoinChangeVariants" should "count the ways to give a change" in {
    countCoinChangeVariants(List(1, 2, 5), 6) shouldBe 5
    countCoinChangeVariants(List(5, 2, 2, 1), 6) shouldBe 5
    countCoinChangeVariants(List(1), 6) shouldBe 1
    countCoinChangeVariants(List(5), 5) shouldBe 1
    countCoinChangeVariants(List(3, 4), 5) shouldBe 0
    countCoinChangeVariants(List(2, 3, 4), 5) shouldBe 1
  }

  it should "return 0 if the change is smaller than all the coins" in {
    countCoinChangeVariants(List(5, 4, 3), 2) shouldBe 0
  }

  "bfsTraversal" should "give the traversed path" in {
    val graph: Int => List[Int] = {
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
    bfsTraversal(1, 2, graph).toSeq shouldBe Seq(1, 2)
    bfsTraversal(1, 5, graph).toSeq shouldBe Seq(1, 2, 5)
    bfsTraversal(1, 9, graph).toSeq shouldBe Seq(1, 2, 5, 8, 3, 6, 9)
    bfsTraversal(1, 1, graph).toSeq shouldBe Seq(1)
  }

  it should "work when there is no path found" in {
    val graph: Int => List[Int] = {
      case 1 => List(2, 3)
      case _ => Nil
    }

    bfsTraversal(1, 4, graph).toSeq shouldBe Seq(1, 2, 3)
  }
}
