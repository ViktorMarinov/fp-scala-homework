package homework3

import scala.concurrent.{ExecutionContext, Future}

trait Retrying {
  def retry[T](f: => Future[T], retries: Int)(implicit ec: ExecutionContext): Future[T] = {
    f recoverWith { case _ if retries > 0 => retry(f, retries - 1) }
  }
}