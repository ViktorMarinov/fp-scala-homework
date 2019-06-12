package homework2

import java.time.LocalDate

import scala.util.Try
import Validated._

case class RegistrationForm(name: String,
                            email: String,
                            password: String,
                            passwordConfirmation: String,
                            birthYear: String,
                            birthMonth: String,
                            birthDay: String,
                            postalCode: String)

sealed trait RegistrationFormError

case object NameIsEmpty extends RegistrationFormError

case class InvalidEmail(email: String) extends RegistrationFormError

case object PasswordTooShort extends RegistrationFormError
case object PasswordRequiresGreaterSymbolVariety extends RegistrationFormError
case object PasswordsDoNotMatch extends RegistrationFormError

case class InvalidBirthdayDate(dateErrors: Chain[DateError]) extends RegistrationFormError
case class BirthdayDateIsInTheFuture(date: Date) extends RegistrationFormError

case class InvalidPostalCode(code: String) extends RegistrationFormError

sealed trait DateError
case class YearIsNotAnInteger(year: String) extends DateError
case class MonthIsNotAnInteger(month: String) extends DateError
case class DayIsNotAnInteger(day: String) extends DateError
case class MonthOutOfRange(month: Int) extends DateError
case class DayOutOfRange(day: Int) extends DateError
case class InvalidDate(year: Int, month: Int, day: Int) extends DateError

case class Email(user: String, domain: String)

case class User(name: String,
                email: Email,
                passwordHash: String,
                birthday: Date,
                postalCode: Option[String])

object DateValidator {
  def toInt(s: String): Option[Int] = Try(s.toInt).toOption

  def validateInt(s: String, onError: => DateError): Validated[DateError, Int] =
    toInt(s).toValidated(onError)

  def validateYearIsInt(year: String): Validated[DateError, Int] =
    validateInt(year, YearIsNotAnInteger(year))

  def validateMonthIsInt(month: String): Validated[DateError, Int] =
    validateInt(month, MonthIsNotAnInteger(month))

  def validateDayIsInt(day: String): Validated[DateError, Int] =
    validateInt(day, DayIsNotAnInteger(day))

  def validateMonthIsInRange(month: Int): Validated[MonthOutOfRange, Int] =
    if (month >= 1 && month <= 12) Valid(month) else Invalid(MonthOutOfRange(month))

  def validateDayIsInRange(day: Int): Validated[DayOutOfRange, Int] =
    if (day >= 1 && day <= 31) Valid(day) else Invalid(DayOutOfRange(day))

  def validateRealDate(year: Int, month: Int, day: Int): Validated[InvalidDate, Date] =
    Date.applyOption(year, month, day).toValidated(InvalidDate(year, month, day))

  def validate(year: String, month: String, day: String): Validated[DateError, Date] = {
    val validateInts: Validated[DateError, (Int, Int, Int)] = (
      validateYearIsInt(year),
      validateMonthIsInt(month).flatMap(validateMonthIsInRange),
      validateDayIsInt(day).flatMap(validateDayIsInRange)
    ).zip

    validateInts.flatMap {
      case (y, m, d) => validateRealDate(y, m, d)
    }
  }
}

object UserRegistration {

  val validEmailRegex = """^([^@]+)@([^@]+)$""".r

  def validateName(str: String) =
    if (str.isEmpty) Invalid(NameIsEmpty) else Valid(str)

  def validateEmail(email: String) = email match {
    case validEmailRegex(user, domain) => Valid(Email(user, domain))
    case _ => Invalid(InvalidEmail(email))
  }

  def validatePasswordLength(password: String) =
    if (password.length >= 8) Valid(password) else Invalid(PasswordTooShort)

  def validatePasswordSymbolVariety(password: String) =
    if (password.exists(_.isLetter) &&
      password.exists(_.isDigit) &&
      password.exists(!_.isLetterOrDigit))
      Valid(password)
    else
      Invalid(PasswordRequiresGreaterSymbolVariety)

  def validatePasswordsMatch(password: String, passwordConfirmation: String) =
    if (password.equals(passwordConfirmation)) Valid(password)
    else Invalid(PasswordsDoNotMatch)

  def validatePassword(password: String, passwordConfirmation: String): Validated[RegistrationFormError, String] = (
    validatePasswordLength(password),
    validatePasswordSymbolVariety(password),
    validatePasswordsMatch(password, passwordConfirmation)
  ).zip.map(_ => password)

  def validateBirthDate(birthDay: String, birthMonth: String, birthYear: String): Validated[InvalidBirthdayDate, Date] =
    DateValidator.validate(birthYear, birthMonth, birthDay) match {
      case Invalid(errors) => Invalid(InvalidBirthdayDate(errors))
      case Valid(date) => Valid(date)
    }

  def validateNotInFuture(date: Date, today: Date) =
    if (LocalDate.of(date.year, date.month, date.day).isAfter(LocalDate.of(today.year, today.month, today.day)))
      Invalid(BirthdayDateIsInTheFuture(date))
    else
      Valid(date)

  def validateDate(birthDay: String, birthMonth: String, birthYear: String, today: Date) =
    for {
      date <- validateBirthDate(birthDay, birthMonth, birthYear)
      validated <- validateNotInFuture(date, today)
    } yield validated

  def validatePostalCode(postalCode: String, userCountryPostalCodeVerifier: String => Boolean): Validated[InvalidPostalCode, Option[String]] = postalCode match {
    case "" => Valid(None)
    case _ => if (userCountryPostalCodeVerifier(postalCode))
                Valid(Some(postalCode))
              else Invalid(InvalidPostalCode(postalCode))
  }

  def registerUser(userCountryPostalCodeVerifier: String => Boolean, today: Date)
                  (form: RegistrationForm): Validated[RegistrationFormError, User] = {

    (
      validateName(form.name),
      validateEmail(form.email),
      validatePassword(form.password, form.passwordConfirmation).map(PasswordUtils.hash(_)),
      validateDate(form.birthDay, form.birthMonth, form.birthYear, today),
      validatePostalCode(form.postalCode, userCountryPostalCodeVerifier)
    ).zipMap(User.apply)
  }

}
