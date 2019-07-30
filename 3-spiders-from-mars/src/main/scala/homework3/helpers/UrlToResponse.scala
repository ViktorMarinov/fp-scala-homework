package homework3.helpers

import homework3.SpideyConfig
import homework3.http.{HttpClient, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

case class ServerError[T](response: T) extends Exception

trait Retrying {
  def retry[T](f: => Future[T], retries: Int)(implicit ec: ExecutionContext): Future[T] = {
    f recoverWith { case _ if retries > 0 => retry(f, retries - 1) }
  }
}

case class UrlResponse(url: String, response: HttpResponse)

trait UrlToResponse {
  def apply(url: String): Future[UrlResponse]
}

object UrlToResponse {
  def create(httpClient: HttpClient, config: SpideyConfig)(implicit ex: ExecutionContext) = {
    config.retriesOnError match {
      case n if n > 0 => new RetryingUrlToResponse(httpClient, n)
      case _ => default(httpClient)
    }
  }

  def default(httpClient: HttpClient)(implicit ec: ExecutionContext): UrlToResponse = (url: String) =>
    httpClient.get(url).map(response => UrlResponse(url, response))
}

class RetryingUrlToResponse(httpClient: HttpClient, retriesOnError: Int)(implicit ex: ExecutionContext)
  extends UrlToResponse with Retrying {

  import homework3.math.RichExtensions._

  def apply(url: String) =
    retryingFetch(retriesOnError)(url).map(response => UrlResponse(url, response))

  private def retryingFetch(retries: Int)(url: String) =
    retry(
      httpClient.get(url)
        .map(response => response.isServerError
          .toOption(throw ServerError(response))
          .getOrElse(response)),
      retries
    ).recoverWith {
      case ServerError(response: HttpResponse) => Future.successful(response)
    }
}