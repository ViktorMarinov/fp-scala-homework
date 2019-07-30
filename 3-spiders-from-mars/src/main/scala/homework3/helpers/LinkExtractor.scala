package homework3.helpers

import homework3.SpideyConfig
import homework3.html.HtmlUtils
import homework3.http.{ContentType, HttpResponse, HttpUtils}

trait LinkExtractor {
  def apply(url: String, response: HttpResponse): List[String]
}
object LinkExtractor {

  import homework3.math.RichExtensions._

  def create(config: SpideyConfig) = {
    config.sameDomainOnly.toOption(default).getOrElse(sameDomainLinkExtractor)
  }

  def isHtml(response: HttpResponse) =
    response.contentType.exists(_.mimeType == ContentType.Html)


  def default: LinkExtractor = (url: String, response: HttpResponse) =>
    isHtml(response).toOption {
      HtmlUtils.linksOf(response.body, url).filter(HttpUtils.isValidHttp)
    }.getOrElse(Nil)

  def sameDomainLinkExtractor: LinkExtractor = (url: String, response: HttpResponse) => {
    isHtml(response).toOption {
      HtmlUtils.linksOf(response.body, url).filter(HttpUtils.isValidHttp)
        .filter(HttpUtils.sameDomain(url, _: String))
    }.getOrElse(Nil)
  }
}