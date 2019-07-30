package homework3.processors

import homework3.Mocks
import org.scalatest.{AsyncFlatSpec, Matchers}

class WordCounterTest extends AsyncFlatSpec with Matchers {

  val wordCounter = WordCounter

  "WordCounter" should "only process successfull responses" in {
    wordCounter("example.com", Mocks.notFoundResponse)
      .map(_.wordToCount.isEmpty shouldBe true)
  }

  it should "process html response" in {
    wordCounter("example.com", Mocks.htmlResponse)
      .map(wordCount => {
        wordCount.wordToCount.isEmpty shouldBe false
        wordCount.wordToCount.contains("html") shouldBe false
        wordCount.wordToCount.contains("body") shouldBe false
        wordCount.wordToCount.get("just") shouldEqual Some(1)
      })
  }

  it should "process plain text response" in {
    wordCounter("example.com", Mocks.plainTextResponse)
      .map(wordCount => {
        wordCount.wordToCount.isEmpty shouldBe false
        wordCount.wordToCount.get("test") shouldEqual Some(2)
      })
  }

  it should "not process other response" in {
    wordCounter("example.com", Mocks.pngResponse)
      .map(_.wordToCount.isEmpty shouldBe true)
  }
}
