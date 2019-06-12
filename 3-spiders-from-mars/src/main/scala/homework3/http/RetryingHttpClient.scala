package homework3.http

import scala.concurrent.{ExecutionContext, Future}

trait Retrying {
  def retry[T](f: => Future[T], retries: Int)(implicit ec: ExecutionContext): Future[T] = {
    f recoverWith {
      case _ if retries > 0 => retry(f, retries - 1)
    }
  }
}

class RetryingHttpClient(retries: Int) extends AsyncHttpClient with Retrying {
  override def get(url: String): Future[HttpResponse] = {

    val f = super.get(url)
        .filter(response => !response.isServerError)

    retry(f, retries)
  }
}
