package homework3.math

import homework3.processors.{FileOutput, SavedFiles, StatusCodeCount, WordCount}

trait Monoid[M] {
  def op(a: M, b: M): M
  def identity: M
}

object Monoid {
  def apply[A](implicit m: Monoid[A]): Monoid[A] = m

  object ops {
    implicit class MonoidOps[A](val a: A) extends AnyVal {
      def |+|(b: A)(implicit m: Monoid[A]) = m.op(a, b)
    }
  }

  implicit val intAdditiveMonoid = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a + b
    val identity: Int = 0
  }

  val intMultiplicativeMonoid = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a * b
    val identity: Int = 1
  }

  implicit val stringMonoid = new Monoid[String] {
    def op(a: String, b: String): String = a + b
    val identity: String = ""
  }

  implicit def optionMonoid[A : Monoid] = new Monoid[Option[A]] {
    import ops._

    def op(a:  Option[A], b:  Option[A]): Option[A] = (a, b) match {
      case (Some(n), Some(m)) => Some(n |+| m)
      case (Some(_), _) => a
      case (_, Some(_)) => b
      case _ => None
    }

    def identity: Option[A] = None
  }

  implicit def pairMonoid[A : Monoid, B : Monoid] = new Monoid[(A, B)] {
    import ops._

    def op(a: (A, B), b: (A, B)): (A, B) = (a, b) match {
      case ((a1, a2), (b1, b2)) => (a1 |+| b1, a2 |+| b2)
    }

    def identity: (A, B) = (Monoid[A].identity, Monoid[B].identity)
  }

  implicit def mapMonoid[K, V : Monoid] = new Monoid[Map[K, V]] {
    import ops._

    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
      val vIdentity = Monoid[V].identity

      (a.keySet ++ b.keySet).foldLeft(identity) { (acc, key) =>
        acc + (key -> (a.getOrElse(key, vIdentity) |+| b.getOrElse(key, vIdentity)))
      }
    }

    def identity: Map[K, V] = Map.empty[K, V]
  }

  implicit def setMonoid[A] = new Monoid[Set[A]] {
    def op(a: Set[A], b: Set[A]): Set[A] = a ++ b
    def identity: Set[A] = Set.empty[A]
  }

  implicit def wordCountMonoid = new Monoid[WordCount] {
    import ops._

    def op(a: WordCount, b: WordCount): WordCount =
      WordCount(a.wordToCount |+| b.wordToCount)

    def identity: WordCount = WordCount(Map.empty)
  }

  implicit def savedFilesMonoid = new Monoid[SavedFiles] {
    def op(a: SavedFiles, b: SavedFiles): SavedFiles = {
      import ops._

      val urlToPath = (a.urlToPath.keySet |+| b.urlToPath.keySet).map { key =>
        key -> (a.urlToPath.getOrElse(key, b.urlToPath(key)))
      }.toMap

      SavedFiles(urlToPath)
    }


    def identity: SavedFiles = SavedFiles(Map.empty)
  }

  implicit def statusCodeCountMonoid = new Monoid[StatusCodeCount] {
    import ops._

    def op(a: StatusCodeCount, b: StatusCodeCount): StatusCodeCount =
      StatusCodeCount(a.codeToCount |+| b.codeToCount)

    def identity: StatusCodeCount = StatusCodeCount(Map.empty)
  }
}