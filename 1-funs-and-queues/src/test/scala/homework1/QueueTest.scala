package homework1

import java.util.NoSuchElementException

import org.scalatest.{FlatSpec, Matchers}

class QueueTest extends FlatSpec with Matchers {
  "an empty queue" should "produce a queue with a single element when that element is added to it" in {
    val emptyQueue = Queue.empty
    val singleElementQueue = emptyQueue.push(42)

    singleElementQueue.peek shouldBe 42
    singleElementQueue.size shouldBe 1
  }

  it should "be immutable" in {
    val emptyQueue = Queue.empty
    val singleElementQueue = emptyQueue.push(42)

    singleElementQueue.pop

    emptyQueue.size shouldBe 0
    singleElementQueue.size shouldBe 1
  }

  "constructors" should "put the elements in the same order" in {
    val q1 = Queue(Seq(1, 2, 3, 4, 5))
    val q2 = Queue.of(1, 2, 3, 4, 5)

    q1.peek shouldBe 1
    q1.peek shouldBe 1
  }

  "peek" should "return the top element" in {
    val emptyQueue = Queue.empty
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
  }

  it should "throw NoSuchElementException if the queue is empty" in {
    assertThrows[NoSuchElementException] {
      Queue.empty.pop
    }
  }
}
