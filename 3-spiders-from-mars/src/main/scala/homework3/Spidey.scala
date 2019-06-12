package homework3

import homework3.html.HtmlUtils
import homework3.http._
import homework3.math.Monoid
import homework3.math.Monoid.ops._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class SpideyConfig(maxDepth: Int,
                        sameDomainOnly: Boolean = true,
                        tolerateErrors: Boolean = true,
                        retriesOnError: Int = 0)

case class UrlResults[O](url: String, response: HttpResponse, result: O) {
  def isHtml = response.contentType.exists(_.mimeType == ContentType.Html)

  def links(config: SpideyConfig) =
    HtmlUtils.linksOf(response.body, url)
      .filter(HttpUtils.isValidHttp)
      .filter { link =>
        if (config.sameDomainOnly) HttpUtils.sameDomain(url, link) else true
      }
}

class Spidey(httpClient: HttpClient)(implicit ex: ExecutionContext) {

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] =
    f.map(Success(_)).recover { case x => Failure(x)}

  class UrlProcessor[O](responseProcessor: Processor[O], retriesOnError: Int) extends Retrying {

    val fetchUrl = retriesOnError match {
      case n if n > 0 => retryingFetch(n) _
      case _ => httpClient.get _
    }

    def process(url: String) = for {
        response <- fetchUrl(url)
        result <- responseProcessor(url, response)
      } yield {
        UrlResults(url, response, result)
      }

    private def retryingFetch(retries: Int)(url: String) = {
      val f = httpClient.get(url)
        .filter(response => !response.isServerError)

      retry(f, retries)
    }
  }

  def crawl[O: Monoid](url: String, config: SpideyConfig)
                      (processor: Processor[O]): Future[O] = {

    if (!HttpUtils.isValidHttp(url)) {
      throw new IllegalArgumentException("Not a valid url")
    }

    val linkExtractor = LinkExtractor.htmlLinkExtractor(config.sameDomainOnly)
    val urlProcessor: UrlProcessor[O] = new UrlProcessor[O](processor, config)

    def crawlRec(urls: Seq[String], maxDepth: Int, visited: Set[String]): Future[Seq[UrlResults[O]]] = {
      val futureResults = urls.map(urlProcessor.process)

      if (config.tolerateErrors) {
        futureResults.map(futureToFutureTry).
      }

      val resultsFuture = Future.sequence(futureResults)

      val nextLevelResults: Future[Seq[UrlResults[O]]] =
        if (maxDepth > 0) {
          resultsFuture.flatMap(results => {
            val uniqueLinks = results
              .filter(_.isHtml)
              .flatMap(_.links(config))
              .distinct
              .filterNot(visited)
            crawlRec(uniqueLinks, maxDepth - 1, visited ++ uniqueLinks)
          })
        } else {
          Future.successful(Nil)
        }

      resultsFuture.zipWith(nextLevelResults)(_ ++ _)
    }

    crawlRec(Seq(url), config.maxDepth, Set(url))
      .map(_.map(_.result).reduce(_ |+| _))
  }
}
