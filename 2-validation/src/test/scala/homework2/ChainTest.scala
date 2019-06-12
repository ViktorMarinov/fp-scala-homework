package homework2

import org.scalatest.{FlatSpec, Matchers}

class ChainTest extends FlatSpec with Matchers {
  "++" should "append two chains" in {
    (Chain(1, 2) ++ Chain(3, 4)) shouldEqual Chain(1, 2, 3, 4)
  }

  "+:" should "append element to a chain" in {
    (Chain(1, 2).+:(3) shouldEqual Chain(3, 1, 2))
  }

  ":+" should "append element to a the end of a chain" in {
    (Chain(1, 2) :+ 3 shouldEqual Chain(1, 2, 3))
  }

  "map" should "apply the function to all elements" in {
    Chain(1,2,3).map(_ + 1) shouldEqual Chain(2,3,4)
    Singleton(1).map(_ * 2) shouldEqual Chain(2)
  }

  "listify" should "make every Chain in the correct format" in {
    val listified = Append(
      Append(Singleton(1), Singleton(2)),
      Append(Singleton(3), Singleton(4))
    ).listify

    listified match {
      case Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4)))) => succeed
      case _ => fail("Chain did not match the correct format")
    }
  }

  it should "work properly with single element Chains" in {
    Singleton(1).listify match {
      case Singleton(1) => succeed
      case _ => fail()
    }
  }

  "reduceLeft" should "work as expected" in {
    Chain(1, 2, 3, 4).reduceLeft(_ + _) shouldEqual 10
  }

  it should "be able to work with not listified Chain" in {
    val chain = Append(
      Append(Singleton(1), Singleton(10)),
      Append(Singleton(19), Singleton(4))
    )

    chain.reduceLeft(_ max _) shouldEqual 19
  }

  "foldLeft" should "be able to work with not listified Chain" in {
    val chain = Append(
      Append(Singleton(1), Singleton(10)),
      Append(Singleton(19), Singleton(4))
    )

    chain.foldLeft(List.empty[Int]){ case (l, x) => x :: l } shouldEqual List(4, 19, 10, 1)
  }

  "min" should "return the min element" in {
    Chain(4, 2, 6, 3, 8).min shouldEqual 2
  }

  val reversedOrdering = new Ordering[Int] {
    override def compare(x: Int, y: Int): Int = y - x
  }

  it should "use the given ordering" in {
    Chain(4, 2, 6, 3, 8).min(reversedOrdering) shouldEqual 8
  }

  "max" should "return the max element" in {
    Chain(4, 2, 6, 3, 8).max shouldEqual 8
  }

  it should "use the given ordering" in {
    Chain(4, 2, 6, 3, 8).max(reversedOrdering) shouldEqual 2
  }

  "equals" should "return true for same chains with different structure" in {
    val chain1 = Append(
      Append(Singleton(1), Singleton(2)),
      Append(Singleton(3), Singleton(4))
    )
    val chain2 = Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4))))

    chain1 shouldEqual chain2
  }
}
