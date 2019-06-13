package homework3

import homework3.http.{ContentType, HttpClient, HttpResponse}

import scala.concurrent.Future

object Mocks {

  def httpClient(urlToResponse: PartialFunction[String, Future[HttpResponse]]) =  new HttpClient {
    def get(url: String): Future[HttpResponse] = urlToResponse(url)
  }

  def successfullHttpClient(urlToResponse: PartialFunction[String, HttpResponse]) = new HttpClient {
    def get(url: String): Future[HttpResponse] = Future.successful(urlToResponse(url))
  }

  def successfullResponseWithLinks(links: String*) =
    Future.successful(makeResponseWithLinks(links:_*))

  def makeResponseWithLinks(links: String*) = new HttpResponse {
    override def headers: Map[String, String] = ???

    override def bodyAsBytes: Array[Byte] = ???

    override def status: Int = 200

    override def body: String = {
      val hrefs = links.map(link => "<a href=\"" + link + "\" />").mkString("\n")
      s"""
         |<html>
         | <body>
         |   <div>
         |     $hrefs
         |   </div>
         | </body>
         |</html>
      """.stripMargin
    }

    override def contentType: Option[ContentType] =
      Some(ContentType(ContentType.Html, None))
  }

  val notFoundResponse = new HttpResponse {
    override def headers: Map[String, String] = ???

    override def bodyAsBytes: Array[Byte] = ???

    override def status: Int = 404
    override def contentType: Option[ContentType] = None
  }

  val okResponse = new HttpResponse {
    override def headers: Map[String, String] = ???

    override def bodyAsBytes: Array[Byte] = ???

    override def body: String = ???

    override def status: Int = 200
  }

  val htmlResponse = new HttpResponse {
    def headers: Map[String, String] = ???
    def bodyAsBytes: Array[Byte] = ???

    def status: Int = 200

    override def body: String =
      """
        |<html>
        | <body>
        |   just some words to be counted
        | </body>
        |</html>
      """.stripMargin

    override def contentType: Option[ContentType] =
      Some(ContentType(ContentType.Html, None))
  }

  val plainTextResponse = new HttpResponse {
    def headers: Map[String, String] = ???
    def bodyAsBytes: Array[Byte] = ???

    def status: Int = 200

    override def body: String =
      """
        |test response body for counting words
        |here we have the word test again
      """.stripMargin

    override def contentType: Option[ContentType] =
      Some(ContentType(ContentType.PlainText, None))
  }

  val pngResponse = new HttpResponse {
    def headers: Map[String, String] = ???
    def bodyAsBytes: Array[Byte] = ???

    def status: Int = 200

    override def contentType: Option[ContentType] =
      Some(ContentType("image/png", None))
  }

  val responseWithLinks = new HttpResponse {
    override def headers: Map[String, String] = ???

    override def bodyAsBytes: Array[Byte] = ???

    override def status: Int = 200

    override def body: String =
      """
        |<html>
        | <body>
            |<link rel="dns-prefetch" href="https://github.githubassets.com">
            |<a href="https://user-images.githubusercontent.com/" />
            |<img src="https://github.githubassets.com/images/search-key-slash.svg" alt="" class="mr-2 header-search-key-slash">
            |<a href="htt://wrong.example.com/" />
        | </body>
        |</html>
      """.stripMargin

    override def contentType: Option[ContentType] =
      Some(ContentType(ContentType.Html, None))
  }

  val serverErrorResponse = new HttpResponse {
    override def status: Int = 500
    override def headers: Map[String, String] = Map.empty
    override def bodyAsBytes: Array[Byte] = Array.emptyByteArray
  }

  def responseWithSameDomainLinks(domain: String) = new HttpResponse {
    override def headers: Map[String, String] = ???

    override def bodyAsBytes: Array[Byte] = ???

    override def status: Int = 200

    override def body: String =
      s"""
        |<html>
        | <body>
        |   <link rel="dns-prefetch" href="https://github.githubassets.com">
        |   <a href="${domain}/users" />
        |   <img src="${domain}/images/search-key-slash.svg" alt="" class="mr-2 header-search-key-slash">
        |   <a href="htt://wrong.example.com/" />
        | </body>
        |</html>
      """.stripMargin

    override def contentType: Option[ContentType] =
      Some(ContentType(ContentType.Html, None))
  }
}
