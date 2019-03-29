package homework1

import java.util.NoSuchElementException

import org.scalatest.{FlatSpec, Matchers}

class QueueTest extends FlatSpec with Matchers {
  "an empty queue" should "produce a queue with a single element when that element is added to it" in {
    val emptyQueue = Queue.empty[Int]
    val singleElementQueue = emptyQueue.push(42)

    singleElementQueue.peek shouldBe 42
    singleElementQueue.size shouldBe 1
  }

  it should "be immutable" in {
    val emptyQueue = Queue.empty[Int]
    val singleElementQueue = emptyQueue.push(42)

    singleElementQueue.pop

    emptyQueue.size shouldBe 0
    singleElementQueue.size shouldBe 1
  }

  "constructors" should "insert the elements in the same order" in {
    val q1 = Queue(Seq(1, 2, 3, 4, 5))
    val q2 = Queue.of(1, 2, 3, 4, 5)

    q1.peek shouldBe 1
    q1.peek shouldBe 1
  }

  "extends" should "insert the elements in the same order " in {
    val q = Queue.empty.extend(Seq(1, 2, 3, 4, 5))

    q.peek shouldBe 1
    q.pop.peek shouldBe 2
    q.pop.pop.peek shouldBe 3
  }

  "peek" should "return the top element" in {
    val emptyQueue = Queue.empty[Int]
    val multielementQueue = emptyQueue.push(42).push(31).push(12)

    multielementQueue.peek shouldBe 42
  }

  it should "throw NoSuchElementException if the queue is empty" in {
    assertThrows[NoSuchElementException] {
      Queue.empty.peek
    }
  }

  "pop" should "shoud return the old queue without the first element" in {
    val queue = Queue.of(1, 2, 3)

    queue.pop.toSeq shouldBe Seq(2, 3)
    queue.pop.pop.toSeq shouldBe Seq(3)
    queue.pop.pop.pop.toSeq shouldBe Seq()
  }

  it should "throw NoSuchElementException if the queue is empty" in {
    assertThrows[NoSuchElementException] {
      Queue.empty.pop
    }
  }

  "isEmpty" should "return true if there are no elements" in {
    val queue = Queue.empty[Int]

    queue.isEmpty shouldBe true
    queue.push(5).pop.isEmpty shouldBe true
  }

  it should "return false if there are any elements" in {
    val queue = Queue.of(3)
    queue.isEmpty shouldBe false
    queue.pop.push(5).isEmpty shouldBe false
  }

  "size" should "return the correct size" in {
    val queue = Queue.empty[Int]

    queue.size shouldBe 0
    queue.push(2).size shouldBe 1
    queue.extend(Seq(2,3,4,5)).size shouldBe 4
    queue.push(3).push(4).pop.size shouldBe 1
  }

  "empty" should "return an empty queue" in {
    Queue.empty.isEmpty shouldBe true
  }
}
