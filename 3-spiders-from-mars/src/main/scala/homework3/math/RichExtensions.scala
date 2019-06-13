package homework3.math

object RichExtensions {

  implicit class RichBoolean(val boolean: Boolean) extends AnyVal {
    def toOption[A](a: => A) = if (boolean) Some(a) else None
  }

}

