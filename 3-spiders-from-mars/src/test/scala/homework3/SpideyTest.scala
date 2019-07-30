package homework3

import java.util.concurrent.TimeUnit

import homework3.processors.StatusCodeCounter
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.Span
import org.scalatest.{AsyncFlatSpec, Matchers}

import org.scalatest.time._
import scala.concurrent.Future

class SpideyTest extends AsyncFlatSpec with Matchers with SpanSugar {

  val domain = "http://www.example.com"
  val config = SpideyConfig(maxDepth = 5, sameDomainOnly = true)
  val httpClient = Mocks.successfullHttpClient {
    case "http://www.example.com" => Mocks.makeResponseWithLinks(
      "http://www.example.com/users",
      "http://www.example.com/page1"
    )
    case "http://www.example.com/users" => Mocks.makeResponseWithLinks(
      "http://www.example.com/page2"
    )
    case "http://www.example.com/page2" => Mocks.makeResponseWithLinks(
      "http://www.example.com/page3"
    )
    case _ => Mocks.notFoundResponse
  }

  "crawl" should "find all links recursively" in {
    val spidey = new Spidey(httpClient)
    spidey.crawl("http://www.example.com", config)(StatusCodeCounter)
      .map(_.codeToCount shouldEqual Map(200 -> 3, 404 -> 2))
  }

  it should "go only to maxDepth level" in {
    val spidey = new Spidey(httpClient)
    val config = SpideyConfig(maxDepth = 1)
    spidey.crawl("http://www.example.com", config)(StatusCodeCounter)
      .map(_.codeToCount shouldEqual Map(200 -> 2, 404 -> 1))
  }

  val httpClientWithFails = Mocks.httpClient {
    case "http://www.example.com" => Mocks.successfullResponseWithLinks(
      "http://www.example.com/users",
      "http://www.example.com/page1"
    )
    case "http://www.example.com/users" =>
      Future.failed(new RuntimeException("network problems"))
    case _ => Future.successful(Mocks.notFoundResponse)
  }

  "with tolerateErrors = false" should "lead to failure on single result fail" in {
    val spidey = new Spidey(httpClientWithFails)
    val config = SpideyConfig(maxDepth = 3, tolerateErrors = false)
    recoverToSucceededIf[RuntimeException] {
      spidey.crawl("http://www.example.com", config)(StatusCodeCounter)
    }
  }

  "with tolerateErrors = true" should "not stop the processing on failures" in {
    val spidey = new Spidey(httpClientWithFails)
    val config = SpideyConfig(maxDepth = 3, tolerateErrors = true)

    spidey.crawl("http://www.example.com", config)(StatusCodeCounter)
        .map(_.codeToCount)
        .map { codeCounts =>
          codeCounts shouldEqual Map(200 -> 1, 404 -> 1)
        }
  }

}