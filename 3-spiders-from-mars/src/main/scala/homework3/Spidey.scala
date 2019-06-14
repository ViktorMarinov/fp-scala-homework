package homework3

import homework3.helpers._
import homework3.http._
import homework3.math.Monoid
import homework3.math.Monoid.ops._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

case class SpideyConfig(maxDepth: Int,
                        sameDomainOnly: Boolean = true,
                        tolerateErrors: Boolean = true,
                        retriesOnError: Int = 0)

class Spidey(httpClient: HttpClient)(implicit ex: ExecutionContext) {

  def crawl[O: Monoid](url: String, config: SpideyConfig)
                      (processor: Processor[O]): Future[O] = {

    if (!HttpUtils.isValidHttp(url)) {
      throw new IllegalArgumentException("Provided url is not a valid http link.")
    }

    val urlToResponse: UrlToResponse = UrlToResponse.create(httpClient, config)
    val responseToResult: ResponseToResult[O] = ResponseToResult.create(processor, config)
    val linkExtractor = LinkExtractor.create(config)

    def crawlRec(urls: Seq[String], maxDepth: Int, visited: Set[String]): Future[Seq[O]] = {
      val futureResponses = urls.map(urlToResponse.apply)
      // can have failed futures

      val futureProcessorResults = futureResponses.map(responseToResult.apply)
      // if tolerant failed futures return identity result
      val resultsFuture = Future.sequence(futureProcessorResults)

      val nextLevelResults = if (maxDepth > 0) {
        resultsFuture.map(_.map {
          case SuccessResult(url , response, _) => linkExtractor(url, response)
          case _ => Nil
        })
          .map(_.flatten.distinct.filterNot(visited))
          .flatMap(links =>
            crawlRec(links, maxDepth - 1, visited ++ links))
      } else {
        Future.successful(Nil)
      }

      val results = resultsFuture.map(_.map(_.result))
      results.zipWith(nextLevelResults)(_ ++ _)
    }

    crawlRec(Seq(url), config.maxDepth, Set(url))
      .map(_.foldLeft(implicitly[Monoid[O]].identity)(_ |+| _))
  }
}
