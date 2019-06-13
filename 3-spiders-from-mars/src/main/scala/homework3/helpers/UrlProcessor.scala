package homework3.helpers

import homework3.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

trait Retrying {
  def retry[T](f: => Future[T], retries: Int)(implicit ec: ExecutionContext): Future[T] = {
    f recoverWith { case _ if retries > 0 => retry(f, retries - 1) }
  }
}

class UrlProcessor[O](httpClient: HttpClient, retriesOnError: Int)(implicit ex: ExecutionContext) extends Retrying {
  val fetchUrl = retriesOnError match {
    case n if n > 0 => retryingFetch(n) _
    case _ => httpClient.get _
  }

  def process(url: String) =
    fetchUrl(url).map(response => UrlResponse(url, response))

  private def retryingFetch(retries: Int)(url: String) = {
    val f = httpClient.get(url)
      .filter(response => !response.isServerError)

    retry(f, retries)
  }
}
