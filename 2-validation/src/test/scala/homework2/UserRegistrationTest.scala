package homework2

import homework2.UserRegistration.registerUser
import org.scalatest.{FlatSpec, Matchers}

class UserRegistrationTest extends FlatSpec with Matchers {
  "An empty form" should "generate errors for the non optional fields" in {
    val emptyForm = RegistrationForm("", "", "", "", "", "", "", "")

    val validation = registerUser(Set.empty, Date(2019, 5, 4))(emptyForm)

    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet = errors.toSet
    val birthdayErrors = errorsSet.collectFirst {
      case InvalidBirthdayDate(dateErrors) => dateErrors.toSet
    }

    errorsSet should have size 5

    errorsSet should contain allOf (
      NameIsEmpty,
      InvalidEmail(""),
      PasswordTooShort,
      PasswordRequiresGreaterSymbolVariety
    )

    birthdayErrors shouldEqual Some(Set(
      YearIsNotAnInteger(""),
      MonthIsNotAnInteger(""),
      DayIsNotAnInteger("")
    ))
  }

  "Form with all wrong values" should "generate all independent errors" in {
    val allWrongForm = RegistrationForm(
      "", "asd@qwe@asd.com", "", "1234", "1990", "II", "-3", "invalid"
    )

    val validation = registerUser(Set.empty, Date(2019, 5, 4))(allWrongForm)

    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet = errors.toSet
    val birthdayErrors = errorsSet.collectFirst {
      case InvalidBirthdayDate(dateErrors) => dateErrors.toSet
    }

    errorsSet should have size 7

    errorsSet should contain allOf (
      NameIsEmpty,
      InvalidEmail("asd@qwe@asd.com"),
      PasswordTooShort,
      PasswordRequiresGreaterSymbolVariety,
      PasswordsDoNotMatch,
      InvalidPostalCode("invalid")
    )

    birthdayErrors shouldEqual Some(Set(
      MonthIsNotAnInteger("II"),
      DayOutOfRange(-3)
    ))
  }

  val validPostalCodes: String => Boolean = Set("1000", "1164", "9000")
  val validForm = RegistrationForm("John", "john@example.com", "John##33", "John##33", "1990", "6", "15", "")

  "An empty postalCode" should "not generate errors" in {
    val validation = registerUser(validPostalCodes, Date(2019, 5, 20))(validForm)
    validation.isValid shouldBe true
  }

  "Non-empty invalid postaCode" should "generate error" in {
    val form = RegistrationForm("John", "john@example.com", "John##33", "John##33", "1990", "6", "15", "1001")
    val validation = registerUser(validPostalCodes, Date(2019, 5, 20))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    errors.toSet should contain (InvalidPostalCode("1001"))
  }

  "DateValidator.validate" should "generate all errors for non-ints" in {
    DateValidator.validate("1233", "12", "5s") shouldEqual Invalid(DayIsNotAnInteger("5s"))
    DateValidator.validate("1233", "1s2", "5") shouldEqual Invalid(MonthIsNotAnInteger("1s2"))
    DateValidator.validate("1233d", "12", "5") shouldEqual Invalid(YearIsNotAnInteger("1233d"))

    DateValidator.validate("1233", "1s2", "5s") shouldEqual Invalid(Chain(
      MonthIsNotAnInteger("1s2"),
      DayIsNotAnInteger("5s")
    ))
    DateValidator.validate("12t33", "1s2", "5s") shouldEqual Invalid(Chain(
      YearIsNotAnInteger("12t33"),
      MonthIsNotAnInteger("1s2"),
      DayIsNotAnInteger("5s")
    ))
  }

  it should "generate all errors for out-of-range ints" in {
    DateValidator.validate("1233", "14", "5") shouldEqual Invalid(MonthOutOfRange(14))
    DateValidator.validate("1233", "12", "-3") shouldEqual Invalid(DayOutOfRange(-3))
  }

}
