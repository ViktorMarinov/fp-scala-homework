package homework3.helpers

import homework3.html.HtmlUtils
import homework3.http.{ContentType, HttpResponse, HttpUtils}

trait LinkExtractor {
  def apply(response: HttpResponse): List[String]
}
object LinkExtractor {
  def isHtml(response: HttpResponse) =
    response.contentType.exists(_.mimeType == ContentType.Html)

  def htmlLinkExtractor(sameDomainOnly: Boolean)(url: String): LinkExtractor = (response: HttpResponse) => {
    if (isHtml(response)) {
      val links = HtmlUtils.linksOf(response.body, url).filter(HttpUtils.isValidHttp)
      if (sameDomainOnly)
        links.filter(HttpUtils.sameDomain(url, _: String))
      else
        links
    } else {
      Nil
    }

  }
}