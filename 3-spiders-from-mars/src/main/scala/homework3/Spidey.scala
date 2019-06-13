package homework3

import homework3.helpers._
import homework3.http._
import homework3.math.Monoid
import homework3.math.Monoid.ops._

import scala.concurrent.{ExecutionContext, Future}

case class SpideyConfig(maxDepth: Int,
                        sameDomainOnly: Boolean = true,
                        tolerateErrors: Boolean = true,
                        retriesOnError: Int = 0)

class Spidey(httpClient: HttpClient)(implicit ex: ExecutionContext) {

  def resultGetter[O](processor: Processor[O])(urlResponse: Future[UrlResponse]): Future[O] =
    urlResponse.flatMap {
      case UrlResponse(url, response) => processor(url, response)
    }

  def crawl[O: Monoid](url: String, config: SpideyConfig)
                      (processor: Processor[O]): Future[O] = {

    if (!HttpUtils.isValidHttp(url)) {
      throw new IllegalArgumentException("Provided url is not a valid http link.")
    }

    val linkExtractor = LinkExtractor.htmlLinkExtractor(config.sameDomainOnly) _
    val urlProcessor: UrlProcessor[O] = new UrlProcessor[O](httpClient, config.retriesOnError)
    val resultGetter: ResultGetter[O] = if (config.tolerateErrors) {
      new TolerantResultGetter[O](processor)
    } else {
      ResultGetter.defaultResultGetter(processor)
    }

    def crawlRec(urls: Seq[String], maxDepth: Int, visited: Set[String]): Future[Seq[O]] = {
      val futureResponses = urls.map(urlProcessor.process)

      val futureResults = futureResponses.map(resultGetter.apply)

      val nextLevelResults = if (maxDepth > 0) {
        val nextLevelLinks = futureResponses.map(_.map {
          case UrlResponse(url, response) => linkExtractor(url)(response)
        })

        Future.sequence(nextLevelLinks)
          .map(_.flatten.distinct.filterNot(visited))
          .flatMap(links =>
            crawlRec(links, maxDepth - 1, visited ++ links))
      } else {
        Future.successful(Nil)
      }

      val results = Future.sequence(futureResults)
      results.zipWith(nextLevelResults)(_ ++ _)
    }

    crawlRec(Seq(url), config.maxDepth, Set(url))
      .map(_.foldLeft(implicitly[Monoid[O]].identity)(_ |+| _))
  }
}
