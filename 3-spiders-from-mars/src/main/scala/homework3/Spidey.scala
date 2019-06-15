package homework3

import cats.data.Chain
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

  def crawl[O: Monoid](url: String, config: SpideyConfig)
                      (processor: Processor[O]): Future[O] = {

    if (!HttpUtils.isValidHttp(url)) {
      throw new IllegalArgumentException("Provided url is not a valid http link.")
    }

    val urlToResponse: UrlToResponse = UrlToResponse.create(httpClient, config)
    val responseToResult: ResponseToResult[O] = ResponseToResult.create(processor, config)
    val linkExtractor = LinkExtractor.create(config)

    def processUrl =
      urlToResponse.apply _ andThen responseToResult.apply

    def traverse(urls: Seq[String], maxDepth: Int, accResults: Chain[O] = Chain.empty, visited: Set[String] = Set.empty): Future[Chain[O]] = {

      val fResults = Future.sequence(urls.map(processUrl))

      fResults.flatMap { readyResults =>
        val newAccResults = accResults ++ Chain.fromSeq(readyResults.map(_.result))

        if (maxDepth <= 0 || readyResults.isEmpty)
          Future.successful(newAccResults)
        else {
          val nextLinks = readyResults.flatMap {
            case SuccessResult(url, response, _) => linkExtractor(url, response)
            case _ => Nil
          }.distinct.filterNot(visited)

          traverse(nextLinks, maxDepth - 1, newAccResults, visited ++ nextLinks)
        }
      }
    }

    val monoidIdentity = implicitly[Monoid[O]].identity
    traverse(Seq(url), config.maxDepth, visited = Set(url)).map { results =>
      results.foldLeft(monoidIdentity)(_ |+| _)
    }
  }
}
