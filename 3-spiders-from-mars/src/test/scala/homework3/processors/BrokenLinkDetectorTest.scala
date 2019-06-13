package homework3.processors

import homework3.Mocks
import org.scalatest.{AsyncFlatSpec, Matchers}

class BrokenLinkDetectorTest extends AsyncFlatSpec with Matchers {

  val brokenLinkDetector = BrokenLinkDetector

  "BrokenLinkDetector" should "include url if status code is 404" in {
    brokenLinkDetector("example.com", Mocks.notFoundResponse)
      .map(urls => {
        urls.size shouldEqual 1
        urls.contains("example.com") shouldBe true
      })
  }

  it should "not include urls with other responses" in {
    brokenLinkDetector("example.com", Mocks.okResponse)
      .map(urls => {
        urls.size shouldEqual 0
        urls.contains("example.com") shouldBe false
      })
  }
}
