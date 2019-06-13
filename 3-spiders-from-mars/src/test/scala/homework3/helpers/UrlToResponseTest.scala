package homework3.helpers

import homework3.Mocks
import homework3.http.{HttpClient, HttpResponse}
import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.Future

class UrlToResponseTest extends AsyncFlatSpec with Matchers {

  val url = "http://www.example.com"

  val mockResponse = Mocks.makeResponseWithLinks(
    "http://www.example.com/users",
    "http://www.example.com/page1"
  )
  val httpClient = Mocks.successfullHttpClient {
    case "http://www.example.com" => mockResponse
    case _ => Mocks.notFoundResponse
  }

  val defaultUrlToResponse = UrlToResponse.default(httpClient)

  "UrlToResponse" should "return the correct response" in {
    defaultUrlToResponse(url)
      .map {
        case UrlResponse(u, r) =>
          u shouldEqual url
          r shouldEqual mockResponse
      }
  }

  "RetryingUrlToResponse" should "retry n times if request fails" in {
    var counter = 0
    val failingHttpClient = new HttpClient {
      override def get(url: String): Future[HttpResponse] = {
        if (counter < 2) {
          counter = counter + 1
          Future.failed(new RuntimeException("network problem"))
        } else {
          Future.successful(Mocks.okResponse)
        }
      }
    }

    val retrying = new RetryingUrlToResponse(failingHttpClient, 2)
    retrying(url)
      .map {
        case UrlResponse(u, r) =>
          u shouldEqual url
          r shouldEqual Mocks.okResponse
      }
  }

  it should "work as normal when no failures" in {
    val retrying = new RetryingUrlToResponse(httpClient, 2)
    retrying(url)
      .map {
        case UrlResponse(u, r) =>
          u shouldEqual url
          r shouldEqual mockResponse
      }
  }

  it should "retry if response is server error" in {
    var counter = 0
    val httpClient = Mocks.successfullHttpClient {
      case _ => if (counter < 2) {
        counter = counter + 1
        Mocks.serverErrorResponse
      } else
        Mocks.okResponse
    }

    val retrying = new RetryingUrlToResponse(httpClient, 2)
    retrying(url)
      .map {
        case UrlResponse(u, r) =>
          u shouldEqual url
          r shouldEqual Mocks.okResponse
      }
  }

  it should "return httpClient.get result on final fail" in {
    var counter = 0
    val numRetries = 2
    val httpClient = Mocks.successfullHttpClient {
      case _ => if (counter < numRetries) {
        counter = counter + 1
        Mocks.serverErrorResponse
      } else
        Mocks.okResponse
    }

    val retrying = new RetryingUrlToResponse(httpClient, numRetries - 1)
    retrying(url)
      .map {
        case UrlResponse(u, r) =>
          u shouldEqual url
          r shouldEqual Mocks.serverErrorResponse
      }
  }



}
