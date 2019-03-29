package homework1

import scala.annotation.tailrec

object Functions {
  def fromDigits(digits: List[Int], radix: Int = 10): Int =
    digits.reverse.zipWithIndex.map {
      case (digit, i) => digit * Math.pow(radix, i).toInt
    }.sum

  def parseInteger(integer: String, radix: Int = 10): Int = {
    val digits = integer.toList
      .filter(_.isLetterOrDigit)
      .map(parseDigit)

    val absValue = fromDigits(digits, radix)
    if (integer.head == '-') -absValue else absValue
  }

  private def parseDigit(c: Char): Int =
    if (c.isDigit) c - '0' else 10 + c - 'A'

  def zipMap(a: List[Int], b: List[Int], f: (Int, Int) => Int): List[Int] =
    a zip b map f.tupled

  def countCoinChangeVariants(denominations: List[Int], change: Int): Int = {
    def loop(denominations: List[Int], change: Int): Int =
      denominations
        .map(denom =>
          denom match {
            case `change`        => 1
            case n if n > change => 0
            case n if n < change =>
              loop(denominations.filter(_ >= denom), change - n)
        })
        .sum

    loop(denominations.distinct.sorted, change)
  }

  def bfsTraversal(start: Int, end: Int, neighbours: Int => List[Int]): Queue[Int] = {
    @tailrec
    def loop(queue: Queue[Int],visited: Set[Int], traversed: Queue[Int]): Queue[Int] = {
      val nextNodes = queue.flatMap(neighbours).filterNot(visited.contains).toSeq.distinct

      if (nextNodes.isEmpty)
        traversed
      else if (nextNodes.exists(_ == end))
        traversed.extend(nextNodes.takeWhile(_ != end)).push(end)
      else
        loop(Queue(nextNodes), visited ++ nextNodes, traversed.extend(nextNodes))
    }

    val startQueue = Queue.of(start)
    if (start == end)
      startQueue
    else
      loop(startQueue, Set(start), startQueue)
  }
}
