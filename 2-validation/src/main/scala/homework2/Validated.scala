package homework2

sealed trait Validated[+E, +A] {
  def isValid: Boolean = this match {
    case Valid(_) => true
    case _ => false
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Valid(a) => a
    case _ => default
  }

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] =
    if (isValid) this else default

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = (this, vb) match {
    case (Valid(a), Valid(b)) => Valid((a, b))
    case (Invalid(ea), Invalid(eb)) => Invalid(ea ++ eb)
    case (Invalid(ea), _) => Invalid(ea)
    case (_, Invalid(eb)) => Invalid(eb)
  }

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(a) => Valid(f(a))
    case _ => this.asInstanceOf[Validated[E, B]]
  }

  def map2[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] = zip(vb).map {
    case (a, b) => f(a, b)
  }

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = this match {
    case Valid(a) => f(a)
    case _ => this.asInstanceOf[Validated[EE, B]]
  }

  def fold[B](invalid: Chain[E] => B, valid: A => B): B = this match {
    case Invalid(errors) => invalid(errors)
    case Valid(a) => valid(a)
  }

  def foreach(f: A => Unit): Unit = fold(_ => (), f)
}

case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))
}

object Validated {
  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] =
    xs.foldLeft(Valid(Nil).asInstanceOf[Validated[E, List[A]]]) {
      case (acc, el) => {
        acc.zip(el).map {
          case (as, a) => a :: as
        }
      }
    }.map(l => l.reverse)

  implicit class ValidatedTuple2[EE, A, B](val tuple: (Validated[EE, A], Validated[EE, B])) extends AnyVal {
    def zip: Validated[EE, (A, B)] = tuple._1 zip tuple._2
    def zipMap[R](f: (A, B) => R): Validated[EE, R] = tuple._1.map2(tuple._2)(f)
  }

  implicit class ValidatedTuple3[EE, A, B, C](val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C])) extends AnyVal {
    def zip: Validated[EE, (A, B, C)] = {
      val (v1, v2, v3) = tuple
      (v1 zip v2 zip v3).map { case ((a, b), c) => (a, b, c) }
    }
    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] = zip.map { case (a, b, c) => f(a, b, c) }
  }

  implicit class ValidatedTuple4[EE, A, B, C, D]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] = {
      val (v1, v2, v3, v4) = tuple
      (v1 zip v2 zip v3 zip v4).map { case (((a, b), c), d) => (a, b, c, d) }
    }
    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] = zip.map { case (a, b, c, d) => f(a, b, c, d) }
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D], Validated[EE, E])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] = {
      val (v1, v2, v3, v4, v5) = tuple
      (v1 zip v2 zip v3 zip v4 zip v5).map { case ((((a, b), c), d), e) => (a, b, c, d, e) }
    }
    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] =
      zip.map { case (a, b, c, d, e) => f(a, b, c, d, e) }
  }

  implicit class ValidatedOption[EE, A](val option: Option[A]) extends AnyVal {
    def toValidated[E](onEmpty: => E): Validated[E, A] =
      option.map(Valid(_)).getOrElse(Invalid(onEmpty))
  }
}
