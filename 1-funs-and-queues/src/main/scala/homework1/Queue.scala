package homework1

import java.util.NoSuchElementException

class Queue[A] private (in: List[A], out: List[A]) extends Iterable[A] {

  def push(elem: A) = new Queue(elem :: in, out)
  def pop: Queue[A] = out match {
    case _ :: xs => new Queue(in, xs)
    case Nil =>
      new Queue(
        Nil,
        in.headOption
          .map(_ => in.reverse.tail)
          .getOrElse(throw new NoSuchElementException))
  }

  def peek: A = out.headOption.getOrElse(in.reverse.head)

  override def iterator: Iterator[A] = out.iterator ++ in.reverse.iterator

  override def isEmpty: Boolean = in.isEmpty && out.isEmpty
  override def size: Int = in.size + out.size

  def extend(ys: Iterable[A]) =
    ys.foldLeft(this)((q, elem) => q.push(elem))
}

object Queue {
  def empty[A]: Queue[A] = Queue(Nil)

  def apply[A](xs: Seq[A]): Queue[A] = new Queue(Nil, xs.toList)

  def of[A](xs: A*) = Queue(xs)
}
