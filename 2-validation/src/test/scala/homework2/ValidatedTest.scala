package homework2

import org.scalatest.{FlatSpec, FunSuite, Matchers}

import Validated._

class ValidatedTest extends FlatSpec with Matchers {
  "zip" should "combine valid instances" in {
    Valid(1).zip(Valid("a")) shouldEqual Valid((1, "a"))
  }

  it should "combine errors from invalid instances" in {
    Invalid(1).zip(Invalid(Chain(2, 3))) shouldEqual Invalid(Chain(1, 2, 3))
  }

  it should "get the error if only of both is invalid" in {
    Valid(1).zip(Invalid(Chain(2, 3))) shouldEqual Invalid(Chain(2, 3))
    Invalid(Chain(3, 4)).zip(Valid(2)) shouldEqual Invalid(Chain(3, 4))
  }

  "isValid" should "return true for valid values" in {
    Valid(1).isValid shouldBe true
  }

  it should "return false for invalid values" in {
    Invalid(3).isValid shouldBe false
  }

  "getOrElse" should "give the value if valid" in {
    Valid(3).getOrElse(4) shouldEqual 3
  }

  it should "give the default if invalid" in {
    Invalid(3).getOrElse(4) shouldEqual 4
  }

  "orElse" should "return the same instance if it's valid" in {
    Valid(3).orElse(Valid(4)) shouldEqual Valid(3)
  }

  it should "return default if the instance is invalid" in {
    Invalid(4).orElse(Valid(5)) shouldEqual Valid(5)
    Invalid(4).orElse(Invalid(5)) shouldEqual Invalid(5)
  }

  "map" should "apply the function only if valid" in {
    val valid: Validated[String, Int] = Valid(3)
    val invalid: Validated[String, Int] = Invalid("Some error")

    valid.map(_ + 5) shouldEqual Valid(8)
    invalid.map(_ + 4) shouldEqual Invalid("Some error")
  }

  "flatMap" should "keep the invalid instance if there is one" in {
    val result = for {
      v1 <- Valid(3).asInstanceOf[Validated[String, Int]]
      v2 <- Invalid("Some error").asInstanceOf[Validated[String, Int]]
      v3 <- Valid(4 + v1 + v2).asInstanceOf[Validated[String, Int]]
    } yield (v3)

    result shouldEqual Invalid("Some error")
  }

  it should "work as expected with valid values" in {
    val result = for {
      v1 <- Valid(3)
      v2 <- Valid(4)
    } yield (v1 + v2)

    result shouldEqual Valid(7)
  }

  it should "stop on the first invalid instance" in {
    val result: Validated[String, Int] = for {
      v1 <- Valid(3)
      v2 <- Invalid("Some error").asInstanceOf[Validated[String, Int]]
      v3 <- Invalid("Another error").asInstanceOf[Validated[String, Int]]
    } yield (v1 + v2 * v3)

    result shouldEqual Invalid("Some error")
  }

  "map2" should "work only on the valid instances" in {
    Valid(1).map2(Valid(2))(_ + _) shouldEqual Valid(3)

    Valid(1).map2(Invalid(Chain(42)))(_ + _) shouldEqual Invalid(Chain(42))
  }

  "ValidatedTuple2" should "construct from tuples" in {
    val result: Validated[Nothing, (Int, String)] = (Valid(1), Valid("2")).zip

    result shouldEqual Valid(1, "2")
  }

  it should "have a zipMap method" in {
    val result: Validated[Nothing, String] = (Valid(1), Valid("2"))
      .zipMap{ case (a, b) => b }

    result shouldEqual Valid("2")
  }

  ".sequence" should "combine values in the same order" in {
    val result = Validated.sequence(List(Valid(1), Valid(2), Valid(3)))
    result shouldEqual Valid(Seq(1, 2, 3))
  }

  it should "return an Invalid if there is at least one" in {
    val result = Validated.sequence(List(Valid(1), Invalid(2), Valid(3)))

    result shouldEqual Invalid(2)
  }

  it should "combine the errors" in {
    val result = Validated.sequence(List(Valid(1), Invalid(2), Invalid(3)))

    result shouldEqual Invalid(Chain(2, 3))
  }


  "ValidatedTuple3" should "construct from tuples" in {
    val result: Validated[Nothing, (Int, String, Boolean)] = (Valid(1), Valid("2"), Valid(true)).zip

    result shouldEqual Valid(1, "2", true)
  }

  it should "have a zipMap method" in {
    val result: Validated[Nothing, String] = (Valid(1), Valid("2"), Valid(true))
      .zipMap{ case (a, b, c) => b }

    result shouldEqual Valid("2")
  }

  it should "combine the errors" in {
    val result = (Valid(1), Invalid("2"), Invalid("3")).zip

    result shouldEqual Invalid(Chain("2", "3"))
  }

  "ValidatedTuple4" should "construct from tuples" in {
    val result: Validated[Nothing, (Int, String, Boolean, Double)] =
      (Valid(1), Valid("2"), Valid(true), Valid(2.0)).zip

    result shouldEqual Valid(1, "2", true, 2.0)
  }

  it should "have a zipMap method" in {
    val result: Validated[Nothing, Int] = (Valid(1), Valid("2"), Valid(true), Valid(2))
      .zipMap{ case (a, b, c, d) => a + d }

    result shouldEqual Valid(3)
  }

  "ValidatedTuple5" should "construct from tuples" in {
    val result: Validated[Nothing, (Int, String, Boolean, Double, String)] =
      (Valid(1), Valid("2"), Valid(true), Valid(2.0), Valid("3")).zip

    result shouldEqual Valid(1, "2", true, 2.0, "3")
  }

  it should "have a zipMap method" in {
    val result: Validated[Nothing, String] =
      (Valid(1), Valid("2"), Valid(true), Valid(2.0), Valid("3"))
        .zipMap{ case (a, b, c, d, e) => b + e }

    result shouldEqual Valid("23")
  }

  "ValidatedOption" should "add toValidated method to Option" in {
    Some(5).toValidated(3) shouldEqual Valid(5)
    None.toValidated(3) shouldEqual Invalid(3)
  }
}
