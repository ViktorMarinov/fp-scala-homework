package homework2

sealed trait Chain[+A] {
  def head: A
  def tail: Option[Chain[A]]

  def isEmpty: Boolean = false

  def +:[B >: A](front: B): Chain[B] = Singleton(front) ++ this

  def :+[B >: A](back: B): Chain[B] = this ++ Singleton(back)

  def ++[B >: A](right: Chain[B]): Chain[B] = Append(this, right)

  def foldLeft[B](initial: B)(f: (B, A) => B): B =
    iterator.toSeq.foldLeft(initial)(f)

  def reduceLeft[B >: A](f: (B, A) => B): B =
    iterator.toSeq.reduceLeft(f)

  def map[B](f: A => B): Chain[B] = tail match {
    case Some(t) => t.foldLeft(Chain(f(head))){ case (acc, el) => acc :+ f(el) }
    case _ => Chain(f(head))
  }

  def flatMap[B](f: A => Chain[B]): Chain[B] = tail match {
    case Some(t) => t.foldLeft(f(head)){ case (acc, el) => acc ++ f(el) }
    case _ => f(head)
  }

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match {
    case c: Chain[_] => iterator.toList == c.iterator.toList
    case _ => false
  }

  override def hashCode: Int = foldLeft(0) {
    _ * 31 + _.hashCode
  }

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] = foldLeft(List.empty[A])((acc, next) => next :: acc).reverse
  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

  def min[B >: A](implicit order: Ordering[B]): B = {
    val f: (B, A) => B = order.min(_, _)
    reduceLeft(f)
  }
  def max[B >: A](implicit order: Ordering[B]): B = {
    val f: (B, A) => B = order.max(_, _)
    reduceLeft(f)
  }

  def listify: Chain[A] = this match {
    case s: Singleton[A] => s
    case Append(Singleton(first), rest) => Append(Singleton(first), rest.listify)
    case _ =>
      val it = iterator
      Chain(it.next(), it.toSeq:_*)
  }

  final def iterator: Iterator[A] = this match {
    case s: Singleton[A] => Iterator.single(s.head)
    case Append(left, right) => left.iterator ++ right.iterator
  }
}

case class Singleton[+A](head: A) extends Chain[A] {
  def tail: Option[Chain[A]] = None
}
case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {
  def head: A = left.head
  def tail: Option[Chain[A]] = left match {
    case Singleton(_) => Some(right)
    case _ => listify.tail
  }
}

object Chain {
  def apply[A](head: A, rest: A*): Chain[A] =
    if (rest.isEmpty) Singleton(head)
    else Append(Singleton(head), Chain(rest.head, rest.tail:_*))

  // Allows Chain to be used in pattern matching
  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)
}
