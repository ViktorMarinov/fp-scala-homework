package homework3.helpers

import homework3.http.HttpResponse
import homework3.math.Monoid
import homework3.{Processor, SpideyConfig}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

sealed trait ProcessorResult[O] {
  def result: O
}
case class SuccessResult[O](url: String, httpResponse: HttpResponse, result: O) extends ProcessorResult[O]
case class FailedFutureResult[O](result: O) extends ProcessorResult[O]

trait ResponseToResult[O] {
  def apply(urlResponse: Future[UrlResponse]): Future[ProcessorResult[O]]
}

object ResponseToResult {

  import homework3.math.RichExtensions._

  def create[O: Monoid](processor: Processor[O], config: SpideyConfig)(implicit ec: ExecutionContext) =
    config.tolerateErrors
      .toOption(new TolerantResponseToResult[O](processor))
      .getOrElse(ResponseToResult.default(processor))

  def default[O](processor: Processor[O])(implicit ec: ExecutionContext): ResponseToResult[O] =
    (response: Future[UrlResponse]) =>
      response.flatMap {
        case UrlResponse(url, response) =>
          processor(url, response).map(result => SuccessResult(url, response, result))

      }
}

class TolerantResponseToResult[O: Monoid](processor: Processor[O])(implicit ec: ExecutionContext)
  extends ResponseToResult[O] {

  private lazy val monoidIdentity = implicitly[Monoid[O]].identity

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] =
    f.map(Success(_)).recover { case x => Failure(x) }

  override def apply(urlResponse: Future[UrlResponse]): Future[ProcessorResult[O]] =
    futureToFutureTry(urlResponse).flatMap {
      case Success(UrlResponse(url, response)) =>
        processor(url, response).map{ result =>
          SuccessResult(url, response, result)
        }.recover { case _ => FailedFutureResult(monoidIdentity) }
      case _ => Future.successful(FailedFutureResult(monoidIdentity))
    }
}
