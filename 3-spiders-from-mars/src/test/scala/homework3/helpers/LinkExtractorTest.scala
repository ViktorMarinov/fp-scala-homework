package homework3.helpers

import homework3.Mocks
import homework3.html.HtmlUtils
import homework3.http.HttpResponse
import org.scalatest.{FlatSpec, Matchers}

class LinkExtractorTest extends FlatSpec with Matchers{
  val url = "http://www.example.com"
  val linkExtractor = LinkExtractor.default(url, _: HttpResponse)
  val sameDomainExtractor = LinkExtractor.sameDomainLinkExtractor(url, _: HttpResponse)

  "linkExtractor" should "return empty list for non-html responses" in {
    linkExtractor(Mocks.pngResponse) shouldEqual Nil
  }

  it should "preserve the same orders as HtmlUtils.linkOf" in {
    val links1 = HtmlUtils.linksOf(Mocks.responseWithLinks.body, url)
    val links2 = linkExtractor(Mocks.responseWithLinks)
    links2 shouldEqual links1.filter(l => links2.contains(l))
  }

  it should "include only valid http links" in {
    linkExtractor(Mocks.responseWithLinks) should contain only (
      "https://github.githubassets.com",
      "https://user-images.githubusercontent.com/",
      "https://github.githubassets.com/images/search-key-slash.svg"
    )
  }

  "sameDomainLinkExtractor" should "return only links within same domain" in {
    sameDomainExtractor(Mocks.responseWithSameDomainLinks("http://www.example.com")) should contain only (
      "http://www.example.com/images/search-key-slash.svg",
      "http://www.example.com/users"
    )
  }
}
