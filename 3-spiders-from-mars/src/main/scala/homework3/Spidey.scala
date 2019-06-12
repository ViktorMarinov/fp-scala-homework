package homework3

import homework3.html.HtmlUtils
import homework3.http._
import homework3.math.Monoid
import homework3.math.Monoid.ops._

import scala.concurrent.{ExecutionContext, Future}

case class SpideyConfig(maxDepth: Int,
                        sameDomainOnly: Boolean = true,
                        tolerateErrors: Boolean = true,
                        retriesOnError: Int = 0)

case class UrlResults[O](url: String, response: HttpResponse, result: O) {
  def isHtml = response.contentType.exists(_.mimeType == ContentType.Html)

  def links =
    if (isHtml)
      HtmlUtils.linksOf(response.body, url)
    else
      Nil
}

class Spidey(httpClient: HttpClient)(implicit ex: ExecutionContext) {

  def crawl[O: Monoid](url: String, config: SpideyConfig)
                      (processor: Processor[O]): Future[O] = {

    if (!HttpUtils.isValidHttp(url)) {
      throw new IllegalArgumentException("Not a valid url")
    }

    def crawlRec(urls: Seq[String], maxDepth: Int, visited: Set[String]): Future[Seq[UrlResults[O]]] = {
      val resultsFuture = Future.sequence(urls.map(processUrl(processor)))

      val nextLevelResults: Future[Seq[UrlResults[O]]] =
        if (maxDepth > 0) {
          resultsFuture.flatMap(results => {
            val uniqueLinks = results.flatMap(_.links).distinct.filterNot(visited)
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

  private def processUrl[O](processor: Processor[O])(url: String) = for {
    response <- httpClient.get(url)
    result <- processor(url, response)
  } yield {
    UrlResults(url, response, result)
  }
}
