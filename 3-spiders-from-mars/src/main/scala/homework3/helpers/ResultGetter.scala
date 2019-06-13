package homework3.helpers

import homework3.math.Monoid
import homework3.Processor

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait ResultGetter[O] {
  def apply(urlResponse: Future[UrlResponse]): Future[O]
}

object ResultGetter {
  def defaultResultGetter[O](processor: Processor[O])(implicit ec: ExecutionContext): ResultGetter[O] =
    (response: Future[UrlResponse]) =>
      response.flatMap {
        case UrlResponse(url, response) => processor(url, response)
    }
}

class TolerantResultGetter[O: Monoid](processor: Processor[O])(implicit ec: ExecutionContext)
    extends ResultGetter[O] {

  private lazy val monoidIdentity = implicitly[Monoid[O]].identity

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] =
    f.map(Success(_)).recover { case x => Failure(x) }

  override def apply(urlResponse: Future[UrlResponse]): Future[O] =
    futureToFutureTry(urlResponse).flatMap {
      case Success(UrlResponse(url, response)) => processor(url, response)
      case _                                   => Future.successful(monoidIdentity)
    }
}
