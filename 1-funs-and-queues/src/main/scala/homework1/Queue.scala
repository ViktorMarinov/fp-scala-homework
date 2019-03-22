package homework1

class Queue(xs: Seq[Int]) extends Iterable[Int] {
  def peek: Int = xs.head

  def push(n: Int): Queue = Queue(xs :+ n)
  def pop: Queue = Queue(xs.tail)

  override def isEmpty: Boolean = xs.isEmpty
  override def size: Int = xs.size

  override def iterator: Iterator[Int] = xs.iterator

  def extend(ys: Iterable[Int]) =
    ys.foldLeft(this)((q, elem) => q.push(elem))
}

object Queue {
  def empty: Queue = Queue(Nil)

  def apply(xs: Seq[Int]): Queue = new Queue(xs)
}
