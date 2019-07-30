package homework3.helpers

import homework3.{Mocks, Processor}
import homework3.http.HttpResponse
import homework3.math.Monoid
import homework3.processors.{StatusCodeCount, StatusCodeCounter}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.Future

class ResponseToResultTest extends AsyncFlatSpec with Matchers {

  val processor = StatusCodeCounter
  val responseToResult = ResponseToResult.default(processor)
  val tolerantResponseToResult = new TolerantResponseToResult(processor)

  val domain = "http://www.example.com"
  val response = Mocks.makeResponseWithLinks(
    "http://www.example.com/users",
    "http://www.example.com/page1"
  )

  "defaultResponseToResult" should "simply apply the processor" in {
    val urlResponse = UrlResponse(domain, response)
    responseToResult.apply(Future.successful(urlResponse))
        .map(_.result.codeToCount)
        .map{ counts =>
          counts.size shouldBe 1
          counts(200) shouldBe 1
        }
  }

  it should "fail on failing future" in {
    val f = responseToResult.apply(Future.failed(new Exception("some forced exception")))
    ScalaFutures.whenReady(f.failed) { e =>
      e shouldBe a [Exception]
      e.getMessage shouldEqual "some forced exception"
    }
  }

  "tolerantResponseToResult" should "tolerate errors from response" in {
    tolerantResponseToResult.apply(Future.failed(new Exception("some forced exception")))
      .map(_.result shouldEqual implicitly[Monoid[StatusCodeCount]].identity)
  }

  it should "tolerate errors from processor" in {
    val processor = new Processor[Int] {
      def apply(url: String, response: HttpResponse): Future[Int] =
        Future.failed(new RuntimeException("example message"))
    }
    val underTest = new TolerantResponseToResult(processor)

    val urlResponse = UrlResponse(domain, response)
    underTest.apply(Future.successful(urlResponse))
        .map(_.result shouldEqual implicitly[Monoid[Int]].identity)
  }
}
