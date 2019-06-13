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

case class UrlResponse(url: String, response: HttpResponse) {
  def isHtml = response.contentType.exists(_.mimeType == ContentType.Html)

  def links(config: SpideyConfig) =
    HtmlUtils.linksOf(response.body, url)
      .filter(HttpUtils.isValidHttp)
      .filter { link =>
        if (config.sameDomainOnly) HttpUtils.sameDomain(url, link) else true
      }
}
case class UrlResponseWithResult[O](url: String, response: HttpResponse, result: O)

class UrlProcessor[O](httpClient: HttpClient, retriesOnError: Int) extends Retrying {
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

class Spidey(httpClient: HttpClient)(implicit ex: ExecutionContext) {

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] =
    f.map(Success(_)).recover { case x => Failure(x)}

  def resultGetter[O](processor: Processor[O])(urlResponse: Future[UrlResponse]): Future[O] =
    urlResponse.flatMap {
      case UrlResponse(url, response) => processor(url, response)
    }

  def crawl[O: Monoid](url: String, config: SpideyConfig)
                      (processor: Processor[O]): Future[O] = {

    if (!HttpUtils.isValidHttp(url)) {
      throw new IllegalArgumentException("Not a valid url")
    }

    val linkExtractor = LinkExtractor.htmlLinkExtractor(config.sameDomainOnly)
    val urlProcessor: UrlProcessor[O] = new UrlProcessor[O](httpClient, config.retriesOnError)

    def crawlRec(urls: Seq[String], maxDepth: Int, visited: Set[String]): Future[Seq[O]] = {
      val futureResults = urls.map(urlProcessor.process)

      val mappedToResults = futureResults.map(resultGetter(processor))
      val results = Future.sequence(mappedToResults)

      val nextLevelLinks = futureResults.map(_.map {
        case UrlResponse(url, response) => linkExtractor(url)(response)
      })

      val nextLevelResults = if (maxDepth > 0) {
        Future.sequence(nextLevelLinks)
          .map(_.flatten.distinct.filterNot(visited))
          .flatMap(links =>
            crawlRec(links, maxDepth - 1, visited ++ links))
      } else {
        Future.successful(Nil)
      }

//      val resultsFuture = Future.sequence(futureResults)
//
//      val nextLevelResults: Future[Seq[O]] =
//        if (maxDepth > 0) {
//          resultsFuture.flatMap(results => {
//            val uniqueLinks = results
//              .filter(_.isHtml)
//              .flatMap(_.links(config))
//              .distinct
//              .filterNot(visited)
//            crawlRec(uniqueLinks, maxDepth - 1, visited ++ uniqueLinks)
//          })
//        } else {
//          Future.successful(Nil)
//        }

      results.zipWith(nextLevelResults)(_ ++ _)
    }

    crawlRec(Seq(url), config.maxDepth, Set(url))
      .map(_.foldLeft(implicitly[Monoid[O]].identity)(_ |+| _))
  }
}
